use std::sync::Arc;

use bincode::{DefaultOptions, Options};
use thiserror::Error;

use crate::arena::{CellArena, CellExtendArena, CellInterner, Id};
use crate::ast::{Expr, File, FnDef, Stmt, Stmts};
use crate::bytecode::{Function, LabelIndex, Module, Opcode, OpcodeIndex, Type};
use crate::error::{Errors, OneOf};
use crate::nameres::{self, Resolved};
use crate::parser::FileParser;
use crate::typecheck::{self, SolvedTypes, TyCheckError};

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
pub enum ParseError {
    #[error("invalid token encountered at {0}")]
    InvalidToken(usize),
    #[error("encountered unexpected EOF")]
    UnrecognizedEof,
    #[error("{0}..{1}: unexpected token {2} encountered; expected {3}")]
    UnrecognizedToken(usize, usize, String, OneOf<String>),
    #[error("unexpected extra token encountered")]
    ExtraToken,
}

pub fn parse_ast(source: String) -> Result<File, ParseError> {
    let source = Arc::new(source);
    let exprs = CellArena::new();
    let exprlists = CellArena::new();
    let spans = CellExtendArena::new();
    let strings = CellInterner::new();
    let stmts = CellArena::new();
    let file = FileParser::new()
        .parse(
            &source, &exprs, &exprlists, &strings, &stmts, &spans, &source,
        )
        .map_err(|err| match err {
            lalrpop_util::ParseError::InvalidToken { location } => {
                ParseError::InvalidToken(location)
            }
            lalrpop_util::ParseError::UnrecognizedEof { .. } => ParseError::UnrecognizedEof,
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (l1, t, l2),
                expected,
            } => ParseError::UnrecognizedToken(
                l1,
                l2,
                format!("`{t}`"),
                OneOf::new(expected).expect("unrecognized token with no expected tokens"),
            ),
            lalrpop_util::ParseError::ExtraToken { .. } => ParseError::ExtraToken,
            _ => unreachable!(),
        })?;
    Ok(file)
}

#[derive(Clone, Debug, Error)]
pub enum CompileError {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    TyCheck(#[from] TyCheckError),
}

pub fn compile_bytecode(source: String) -> Result<Module, Errors<CompileError>> {
    let file = parse_ast(source).map_err(CompileError::Parse)?;
    let resolved = nameres::resolve_names(&file);
    let solved = typecheck::check_types(&file, &resolved).map_err(Errors::into)?;
    let funcs = compile_file(&file, &resolved, &solved);
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

pub fn compile_file(file: &File, resolved: &Resolved, solved: &SolvedTypes) -> Vec<Function> {
    let mut stack = LabelScopeStack::new();
    stack.push(LabelScope::top_level());

    file.defs()
        .iter()
        .map(|def| compile_func(file, resolved, solved, def, &mut stack))
        .collect::<Vec<Function>>()
}

fn compile_func(
    file: &File,
    resolved: &Resolved,
    solved: &SolvedTypes,
    def: &FnDef,
    stack: &mut LabelScopeStack,
) -> Function {
    let mut func = Function {
        name: file.str(def.name()).to_string(),
        opcodes: vec![],
        labels: vec![OpcodeIndex::UNSET],
        label_pool: vec![],
        locals: vec![Type::Void; resolved.local_len(def.name())],
        params: resolved.params(def.name()).to_vec(),
        return_type: resolved.return_type(def.name()),
    };

    compile_stmts(
        file,
        resolved,
        solved,
        file.stmts(def.body()),
        &mut func,
        stack,
    );

    func.labels[0] = OpcodeIndex::new(func.opcodes.len());
    func
}

fn compile_stmts(
    file: &File,
    resolved: &Resolved,
    solved: &SolvedTypes,
    stmts: &Stmts,
    func: &mut Function,
    scopes: &mut LabelScopeStack,
) {
    for stmt in stmts.stmts().iter().copied() {
        match stmt {
            Stmt::Expr(expr) => compile_expr(file, resolved, solved, expr, func, scopes),
        }
    }
}

fn compile_expr(
    file: &File,
    resolved: &Resolved,
    solved: &SolvedTypes,
    expr: Id<Expr>,
    func: &mut Function,
    scopes: &mut LabelScopeStack,
) {
    match file.expr(expr) {
        Expr::Let { value, .. } => {
            compile_expr(file, resolved, solved, value, func, scopes);
            let local = resolved.local(expr);
            let ty = solved.get(value);
            func.locals[usize::try_from(local).unwrap()] = ty;
            func.push_op(Opcode::StoreLocal(local));
        }
        Expr::Set { value, .. } => {
            compile_expr(file, resolved, solved, value, func, scopes);
            let local = resolved.local(expr);
            func.push_op(Opcode::StoreLocal(local));
        }
        Expr::Name(_) => {
            let local = resolved.local(expr);
            func.push_op(Opcode::LoadLocal(local));
        }
        Expr::Literal(constant) => {
            func.push_op(Opcode::LiteralS4(constant));
        }
        Expr::Plus(lhs, rhs) => {
            compile_expr(file, resolved, solved, lhs, func, scopes);
            compile_expr(file, resolved, solved, rhs, func, scopes);
            func.push_op(Opcode::Add);
        }
        Expr::Minus(lhs, rhs) => {
            compile_expr(file, resolved, solved, lhs, func, scopes);
            compile_expr(file, resolved, solved, rhs, func, scopes);
            func.push_op(Opcode::Sub);
        }
        Expr::Times(lhs, rhs) => {
            compile_expr(file, resolved, solved, lhs, func, scopes);
            compile_expr(file, resolved, solved, rhs, func, scopes);
            func.push_op(Opcode::Mult);
        }
        Expr::While { pred, body } => {
            let before_loop = func.add_label();
            let after_loop = func.add_label();
            let targets = func.push_label_slice(&[after_loop]);
            let fallthrough = func.add_label();

            func.set_label(before_loop, OpcodeIndex::new(func.ops().len()));

            compile_expr(file, resolved, solved, pred, func, scopes);
            func.push_op(Opcode::Branch {
                default: fallthrough,
                targets,
            });

            func.set_label(fallthrough, OpcodeIndex::new(func.ops().len()));

            scopes.push(LabelScope::new(after_loop));
            compile_stmts(file, resolved, solved, file.stmts(body), func, scopes);
            func.push_op(Opcode::Jump(before_loop));

            scopes.pop();
            func.set_label(after_loop, OpcodeIndex::new(func.ops().len()));
        }
        Expr::Break(val) => {
            // TODO: Non-empty breaks
            _ = val;
            // TODO: Good error on break without scope
            // TODO: Actually break from a loop and not whatever
            let label = scopes.top().end;
            func.push_op(Opcode::Jump(label));
        }
        Expr::Return(val) => {
            // TODO: Empty returns
            let val = val.unwrap();
            compile_expr(file, resolved, solved, val, func, scopes);
            func.push_op(Opcode::Return);
        }
        Expr::Call { params, .. } => {
            todo!()
        }
    }
}

#[cfg(test)]
mod tests {}
