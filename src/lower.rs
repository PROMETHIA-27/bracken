use std::cell::RefCell;

use bincode::{DefaultOptions, Options};
use comemo::{memoize, Track, Tracked};
use thiserror::Error;

use crate::arena::IndexedArena;
use crate::ast::{Ast, Expr, FnDef, Stmt, Stmts};
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

// #[memoize]
pub fn parse_ast<'ast>(source: String) -> Result<Ast<'ast>, ParseError> {
    let arena = RefCell::new(IndexedArena::new());
    let file = FileParser::new()
        .parse(&arena, &source)
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
    Ok(Ast::new(arena.into_inner(), vec![file]))
}

#[memoize]
pub fn compute_bytecode(source: String) -> Result<Module, Errors<ParseError>> {
    let ast = parse_ast(source)?;
    let funcs = ast.files()[0]
        .defs()
        .iter()
        .map(|def| compile_func(def.track()))
        .collect::<Vec<Function>>();
    Ok(Module { funcs })
}

#[memoize]
pub fn compile_func(def: Tracked<FnDef>) -> Function {
    let mut func = Function {
        name: def.name().to_string(),
        opcodes: vec![],
        labels: vec![OpcodeIndex::UNSET],
        label_pool: vec![],
        locals: vec![],
    };
    let mut stack = ScopeStack::new();
    stack.push(Scope::top_level_scope());

    compile_stmts(def.body(), &mut func, &mut stack);

    func.labels[0] = OpcodeIndex::new(func.opcodes.len());
    func
}

pub fn compile_stmts<'f>(stmts: &'f Stmts, func: &mut Function, scopes: &mut ScopeStack<'f>) {
    for stmt in stmts.stmts() {
        match stmt {
            Stmt::Expr(expr) => compile_expr(expr, func, scopes),
        }
    }
}

pub fn compile_expr<'f>(expr: &'f Expr, func: &mut Function, scopes: &mut ScopeStack<'f>) {
    match expr {
        Expr::Let { name, value, .. } => {
            compile_expr(value, func, scopes);
            let local = func.locals.len().try_into().unwrap();
            scopes.add_local(name, local);
            func.locals.push(type_of_expr(value, func, scopes.top()));
            func.push_op(Opcode::StoreLocal(local));
        }
        Expr::Set { name, value } => {
            compile_expr(value, func, scopes);
            let local = scopes.local(name).unwrap();
            func.push_op(Opcode::StoreLocal(local));
        }
        Expr::Local(name) => {
            let local = scopes.local(name).expect("unbound name");
            func.push_op(Opcode::LoadLocal(local));
        }
        &Expr::Literal(constant) => {
            func.push_op(Opcode::LiteralS4(constant));
        }
        Expr::Plus(lhs, rhs) => {
            compile_expr(lhs, func, scopes);
            compile_expr(rhs, func, scopes);
            func.push_op(Opcode::Add);
        }
        Expr::Minus(lhs, rhs) => {
            compile_expr(lhs, func, scopes);
            compile_expr(rhs, func, scopes);
            func.push_op(Opcode::Sub);
        }
        Expr::Times(lhs, rhs) => {
            compile_expr(lhs, func, scopes);
            compile_expr(rhs, func, scopes);
            func.push_op(Opcode::Mult);
        }
        Expr::While { pred, body } => {
            let before_loop = func.add_label();
            let after_loop = func.add_label();
            let targets = func.push_label_slice(&[after_loop]);
            let fallthrough = func.add_label();

            func.set_label(before_loop, OpcodeIndex::new(func.ops().len()));

            compile_expr(pred, func, scopes);
            func.push_op(Opcode::Branch {
                default: fallthrough,
                targets,
            });

            func.set_label(fallthrough, OpcodeIndex::new(func.ops().len()));

            scopes.push(Scope::new(after_loop));
            compile_stmts(body, func, scopes);
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
            let val = val.as_ref().unwrap();
            compile_expr(val, func, scopes);
            func.push_op(Opcode::Return);
        }
    }
}

fn type_of_expr(expr: &Expr, func: &Function, scope: &Scope) -> Type {
    match expr {
        Expr::Local(local) => {
            let local = *scope.items().get(local.as_str()).unwrap();
            *func
                .locals
                .get::<usize>(local.local().try_into().unwrap())
                .unwrap()
        }
        Expr::Literal(_) => Type::S4,
        Expr::Plus(l, r) | Expr::Times(l, r) | Expr::Minus(l, r) => {
            let l = type_of_expr(l, func, scope);
            let r = type_of_expr(r, func, scope);
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
