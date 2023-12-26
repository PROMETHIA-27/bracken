use bincode::{DefaultOptions, Options};
use comemo::{memoize, Track, Tracked};
use thiserror::Error;

use crate::arena::{CellArena, CellInterner, Id};
use crate::ast::{Expr, File, FnDef, Stmt, Stmts};
use crate::bytecode::{Function, Module, Opcode, OpcodeIndex, Type};
use crate::error::{Errors, OneOf};
use crate::nameres::{Scope, ScopeStack};
use crate::parser::FileParser;

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Serialize(#[from] SerError),
}

#[derive(Clone, Debug, Error)]
pub enum SerError {
    #[error("serialization failed")]
    Failed,
}

#[memoize]
pub fn compile(source: String) -> Result<Vec<u8>, Errors<Error>> {
    let module = compute_bytecode(source)
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

#[memoize]
pub fn parse_ast(source: String) -> Result<File, ParseError> {
    let exprs = CellArena::new();
    let strings = CellInterner::new();
    let stmts = CellArena::new();
    let file = FileParser::new()
        .parse(&exprs, &strings, &stmts, &source)
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

#[memoize]
pub fn compute_bytecode(source: String) -> Result<Module, Errors<ParseError>> {
    let file = parse_ast(source)?;
    let funcs = file
        .defs()
        .iter()
        .map(|def| compile_func(&file, def.track()))
        .collect::<Vec<Function>>();
    Ok(Module { funcs })
}

#[memoize]
pub fn compile_func(file: &File, def: Tracked<FnDef>) -> Function {
    let mut func = Function {
        name: file.str(def.name()).to_string(),
        opcodes: vec![],
        labels: vec![OpcodeIndex::UNSET],
        label_pool: vec![],
        locals: vec![],
    };
    let mut stack = ScopeStack::new();
    stack.push(Scope::top_level_scope());

    compile_stmts(file, file.stmts(def.body()), &mut func, &mut stack);

    func.labels[0] = OpcodeIndex::new(func.opcodes.len());
    func
}

pub fn compile_stmts<'f>(
    file: &'f File,
    stmts: &'f Stmts,
    func: &mut Function,
    scopes: &mut ScopeStack<'f>,
) {
    for stmt in stmts.stmts().iter().copied() {
        match stmt {
            Stmt::Expr(expr) => compile_expr(file, expr, func, scopes),
        }
    }
}

pub fn compile_expr<'f>(
    file: &'f File,
    expr: Id<Expr>,
    func: &mut Function,
    scopes: &mut ScopeStack<'f>,
) {
    match file.expr(expr) {
        Expr::Let { name, value, .. } => {
            compile_expr(file, value, func, scopes);
            let local = func.locals.len().try_into().unwrap();
            scopes.add_local(file.str(name), local);
            func.locals
                .push(type_of_expr(file, file.expr(value), func, scopes.top()));
            func.push_op(Opcode::StoreLocal(local));
        }
        Expr::Set { name, value } => {
            compile_expr(file, value, func, scopes);
            let local = scopes.local(file.str(name)).unwrap();
            func.push_op(Opcode::StoreLocal(local));
        }
        Expr::Local(name) => {
            let local = scopes.local(file.str(name)).expect("unbound name");
            func.push_op(Opcode::LoadLocal(local));
        }
        Expr::Literal(constant) => {
            func.push_op(Opcode::LiteralS4(constant));
        }
        Expr::Plus(lhs, rhs) => {
            compile_expr(file, lhs, func, scopes);
            compile_expr(file, rhs, func, scopes);
            func.push_op(Opcode::Add);
        }
        Expr::Minus(lhs, rhs) => {
            compile_expr(file, lhs, func, scopes);
            compile_expr(file, rhs, func, scopes);
            func.push_op(Opcode::Sub);
        }
        Expr::Times(lhs, rhs) => {
            compile_expr(file, lhs, func, scopes);
            compile_expr(file, rhs, func, scopes);
            func.push_op(Opcode::Mult);
        }
        Expr::While { pred, body } => {
            let before_loop = func.add_label();
            let after_loop = func.add_label();
            let targets = func.push_label_slice(&[after_loop]);
            let fallthrough = func.add_label();

            func.set_label(before_loop, OpcodeIndex::new(func.ops().len()));

            compile_expr(file, pred, func, scopes);
            func.push_op(Opcode::Branch {
                default: fallthrough,
                targets,
            });

            func.set_label(fallthrough, OpcodeIndex::new(func.ops().len()));

            scopes.push(Scope::new(after_loop));
            compile_stmts(file, file.stmts(body), func, scopes);
            func.push_op(Opcode::Jump(before_loop));

            scopes.pop();
            func.set_label(after_loop, OpcodeIndex::new(func.ops().len()));
        }
        Expr::Break(val) => {
            // TODO: Non-empty breaks
            _ = val;
            // TODO: Good error on break without scope
            let label = scopes.top().end();
            func.push_op(Opcode::Jump(label));
        }
        Expr::Return(val) => {
            // TODO: Empty returns
            let val = val.unwrap();
            compile_expr(file, val, func, scopes);
            func.push_op(Opcode::Return);
        }
    }
}

fn type_of_expr(file: &File, expr: Expr, func: &Function, scope: &Scope) -> Type {
    match expr {
        Expr::Local(local) => {
            let local = *scope.items().get(file.str(local)).unwrap();
            *func
                .locals
                .get::<usize>(local.local().try_into().unwrap())
                .unwrap()
        }
        Expr::Literal(_) => Type::S4,
        Expr::Plus(l, r) | Expr::Times(l, r) | Expr::Minus(l, r) => {
            let l = type_of_expr(file, file.expr(l), func, scope);
            let r = type_of_expr(file, file.expr(r), func, scope);
            assert_eq!(l, r);
            l
        }
        Expr::Let { .. }
        | Expr::Set { .. }
        | Expr::While { .. }
        | Expr::Break(_)
        | Expr::Return(_) => Type::Void,
    }
}

#[cfg(test)]
mod tests {}
