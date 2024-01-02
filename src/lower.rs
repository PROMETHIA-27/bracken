use bincode::{DefaultOptions, Options};
use thiserror::Error;

use crate::ast::{self, Expr, ExprKind, File, FnDef, ParseError, SourceFile, Stmt, Stmts};
use crate::bytecode::{Function, LabelIndex, Module, Opcode, OpcodeIndex, Type};
use crate::error::Errors;
use crate::nameres::{self, Resolved};
use crate::typecheck::{self, SolvedTypes, TyCheckError};
use crate::{Database, Db};

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] CompileError),
    #[error(transparent)]
    Serialize(#[from] SerError),
}

#[derive(Clone, Debug, Error)]
pub enum SerError {
    #[error("serialization failed")]
    Failed,
}

pub fn compile(source: String) -> Result<Vec<u8>, Errors<Error>> {
    let module = compile_bytecode(source)
        .map_err(|errs| errs.into_iter().map(Error::from).collect::<Vec<_>>())?;
    Ok(DefaultOptions::new()
        .with_varint_encoding()
        .allow_trailing_bytes()
        .serialize(&module)
        .map_err(|_| SerError::Failed)
        .map_err(Error::Serialize)?)
}

#[derive(Clone, Debug, Error)]
pub enum CompileError {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    TyCheck(#[from] TyCheckError),
}

pub fn compile_bytecode(source: String) -> Result<Module, Errors<CompileError>> {
    let db = Database::default();
    let file = ast::parse_file(&db, SourceFile::new(&db, source)).map_err(CompileError::Parse)?;
    let resolved = nameres::resolve_names(&db, &file);
    let solved = typecheck::check_types(&db, &file, &resolved).map_err(Errors::into)?;
    let funcs = compile_file(&db, &file, &resolved, &solved);
    Ok(Module { funcs })
}

struct LabelScopeStack {
    vec: Vec<LabelScope>,
}

impl LabelScopeStack {
    fn new() -> Self {
        Self { vec: vec![] }
    }

    fn push(&mut self, scope: LabelScope) {
        self.vec.push(scope);
    }

    fn pop(&mut self) {
        self.vec.pop().unwrap();
    }

    fn top(&self) -> LabelScope {
        *self.vec.last().unwrap()
    }
}

#[derive(Clone, Copy)]
struct LabelScope {
    end: LabelIndex,
}

impl LabelScope {
    fn new(end: LabelIndex) -> Self {
        LabelScope { end }
    }

    fn top_level() -> Self {
        LabelScope {
            end: LabelIndex::new(0),
        }
    }
}

pub fn compile_file(
    db: &dyn Db,
    file: &File,
    resolved: &Resolved,
    solved: &SolvedTypes,
) -> Vec<Function> {
    let mut stack = LabelScopeStack::new();
    stack.push(LabelScope::top_level());

    file.defs(db)
        .iter()
        .map(|def| compile_func(db, file, resolved, solved, def, &mut stack))
        .collect::<Vec<Function>>()
}

fn compile_func(
    db: &dyn Db,
    file: &File,
    resolved: &Resolved,
    solved: &SolvedTypes,
    def: &FnDef,
    stack: &mut LabelScopeStack,
) -> Function {
    let mut func = Function {
        name: def.name.text(db).to_string(),
        opcodes: vec![],
        labels: vec![OpcodeIndex::UNSET],
        label_pool: vec![],
        locals: vec![Type::Void; resolved.local_len(def.name)],
        params: resolved.params(def.name).to_vec(),
        return_type: resolved.return_type(def.name),
    };

    compile_stmts(db, file, resolved, solved, def.body, &mut func, stack);

    func.labels[0] = OpcodeIndex::new(func.opcodes.len());
    func
}

fn compile_stmts(
    db: &dyn Db,
    file: &File,
    resolved: &Resolved,
    solved: &SolvedTypes,
    stmts: Stmts,
    func: &mut Function,
    scopes: &mut LabelScopeStack,
) {
    for stmt in stmts.stmts(db).iter().copied() {
        match stmt {
            Stmt::Expr(expr) => compile_expr(db, file, resolved, solved, expr, func, scopes),
        }
    }
}

fn compile_expr(
    db: &dyn Db,
    file: &File,
    resolved: &Resolved,
    solved: &SolvedTypes,
    expr: Expr,
    func: &mut Function,
    scopes: &mut LabelScopeStack,
) {
    match expr.kind(db) {
        ExprKind::Let { value, .. } => {
            compile_expr(db, file, resolved, solved, value, func, scopes);
            let local = resolved.local(expr);
            let ty = solved.get(value);
            func.locals[usize::try_from(local).unwrap()] = ty;
            func.push_op(Opcode::StoreLocal(local));
        }
        ExprKind::Set { value, .. } => {
            compile_expr(db, file, resolved, solved, value, func, scopes);
            let local = resolved.local(expr);
            func.push_op(Opcode::StoreLocal(local));
        }
        ExprKind::Name(_) => {
            let local = resolved.local(expr);
            func.push_op(Opcode::LoadLocal(local));
        }
        ExprKind::Literal(constant) => {
            func.push_op(Opcode::LiteralS4(constant));
        }
        ExprKind::Plus(lhs, rhs) => {
            compile_expr(db, file, resolved, solved, lhs, func, scopes);
            compile_expr(db, file, resolved, solved, rhs, func, scopes);
            func.push_op(Opcode::Add);
        }
        ExprKind::Minus(lhs, rhs) => {
            compile_expr(db, file, resolved, solved, lhs, func, scopes);
            compile_expr(db, file, resolved, solved, rhs, func, scopes);
            func.push_op(Opcode::Sub);
        }
        ExprKind::Times(lhs, rhs) => {
            compile_expr(db, file, resolved, solved, lhs, func, scopes);
            compile_expr(db, file, resolved, solved, rhs, func, scopes);
            func.push_op(Opcode::Mult);
        }
        ExprKind::While { pred, body } => {
            let before_loop = func.add_label();
            let after_loop = func.add_label();
            let targets = func.push_label_slice(&[after_loop]);
            let fallthrough = func.add_label();

            func.set_label(before_loop, OpcodeIndex::new(func.ops().len()));

            compile_expr(db, file, resolved, solved, pred, func, scopes);
            func.push_op(Opcode::Branch {
                default: fallthrough,
                targets,
            });

            func.set_label(fallthrough, OpcodeIndex::new(func.ops().len()));

            scopes.push(LabelScope::new(after_loop));
            compile_stmts(db, file, resolved, solved, body, func, scopes);
            func.push_op(Opcode::Jump(before_loop));

            scopes.pop();
            func.set_label(after_loop, OpcodeIndex::new(func.ops().len()));
        }
        ExprKind::Break(val) => {
            // TODO: Non-empty breaks
            _ = val;
            // TODO: Good error on break without scope
            // TODO: Actually break from a loop and not whatever
            let label = scopes.top().end;
            func.push_op(Opcode::Jump(label));
        }
        ExprKind::Return(val) => {
            // TODO: Empty returns
            let val = val.unwrap();
            compile_expr(db, file, resolved, solved, val, func, scopes);
            func.push_op(Opcode::Return);
        }
        ExprKind::Call { params, .. } => {
            for &param in params.exprs(db) {
                compile_expr(db, file, resolved, solved, param, func, scopes);
            }

            let callee = resolved.callee(expr);
            let callee = file.def_id(db, callee);
            func.push_op(Opcode::Call(callee));
        }
    }
}

#[cfg(test)]
mod tests {}
