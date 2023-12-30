use std::ops::Range;
use std::sync::Arc;

use thiserror::Error;

use crate::arena::{ExtendArena, Id};
use crate::ast::{Expr, File, FnDef, Location, Stmt, Stmts};
use crate::bytecode::Type;
use crate::error::Errors;
use crate::nameres::Resolved;

pub struct SolvedTypes {
    tys: ExtendArena<Expr, Type>,
}

impl SolvedTypes {
    pub fn get(&self, expr: Id<Expr>) -> Type {
        *self
            .tys
            .get_no_resize(expr)
            .expect("failed to solve all types but did not detect it")
    }
}

#[derive(Clone, Copy)]
pub enum Constraint {
    Same(Id<Expr>, Id<Expr>),
}

#[derive(Clone, Debug, Error)]
pub enum TyCheckError {
    #[error("insufficient information to determine type of expression `{}` at {}", &.0[.1.clone()], .2)]
    InsufficientInfo(Arc<String>, Range<usize>, Location),
    #[error("type mismatch")]
    TypeMismatch,
}

struct LocalValues {
    values: Vec<Option<Id<Expr>>>,
}

impl LocalValues {
    fn new() -> Self {
        Self { values: vec![] }
    }

    fn get_or_init(&mut self, local: u32, value: Id<Expr>) -> Option<Id<Expr>> {
        let local: usize = local.try_into().unwrap();
        if local >= self.values.len() {
            self.values.resize(local + 1, None);
        }

        match self.values[local] {
            Some(value) => Some(value),
            None => {
                self.values[local] = Some(value);
                None
            }
        }
    }
}

pub fn check_types(file: &File, resolved: &Resolved) -> Result<SolvedTypes, Errors<TyCheckError>> {
    let mut solved = ExtendArena::<Expr, Option<Type>>::new();
    let mut errors = Errors::new();

    for def in file.defs() {
        check_fn_types(file, def, resolved, &mut solved, &mut errors);
    }

    if errors.is_empty() {
        Ok(SolvedTypes {
            tys: solved.map(Option::unwrap),
        })
    } else {
        Err(errors)
    }
}

fn check_fn_types(
    file: &File,
    def: &FnDef,
    resolved: &Resolved,
    solved: &mut ExtendArena<Expr, Option<Type>>,
    errors: &mut Errors<TyCheckError>,
) {
    let mut constraints = vec![];
    let mut locals = LocalValues::new();

    check_stmts_types(
        file,
        def,
        def.body(),
        resolved,
        solved,
        &mut constraints,
        &mut locals,
    );

    let mut i = 0;
    let mut changed = false;
    let mut exit = false;
    while !exit {
        match constraints.get(i) {
            Some(&Constraint::Same(x, y)) => {
                let (x, y) = order_by_solved(x, y, solved);
                let (xs, ys) = (solved.get(x).is_some(), solved.get(y).is_some());
                if xs && ys {
                    // both solved
                    let (xty, yty) = (solved.get(x).unwrap(), solved.get(y).unwrap());
                    if xty != yty {
                        errors.push(TyCheckError::TypeMismatch);
                    }
                    constraints.swap_remove(i);
                    changed = true;
                } else if xs && !ys {
                    // x solved, y unsolved
                    let xty = solved.get(x).unwrap();
                    solved.must_be(y, xty).unwrap();
                    constraints.swap_remove(i);
                    changed = true;
                } else {
                    // both unsolved
                    i += 1;
                    if i == constraints.len() {
                        if changed {
                            i = 0;
                            changed = false;
                        } else {
                            exit = true;
                        }
                    }
                }
            }
            None => exit = true,
        }
    }

    for constraint in constraints {
        match constraint {
            Constraint::Same(x, y) => {
                errors.push(TyCheckError::InsufficientInfo(
                    file.source().clone(),
                    file.span(x),
                    file.offset_to_loc(file.span(x).start),
                ));
                errors.push(TyCheckError::InsufficientInfo(
                    file.source().clone(),
                    file.span(y),
                    file.offset_to_loc(file.span(x).start),
                ));
            }
        }
    }
}

fn check_stmts_types(
    file: &File,
    def: &FnDef,
    stmts: Id<Stmts>,
    resolved: &Resolved,
    solved: &mut ExtendArena<Expr, Option<Type>>,
    constraints: &mut Vec<Constraint>,
    locals: &mut LocalValues,
) {
    for stmt in file.stmts(stmts).stmts() {
        match stmt {
            &Stmt::Expr(expr) => {
                check_expr_types(file, def, expr, resolved, solved, constraints, locals)
            }
        }
    }
}

fn check_expr_types(
    file: &File,
    def: &FnDef,
    expr: Id<Expr>,
    resolved: &Resolved,
    solved: &mut ExtendArena<Expr, Option<Type>>,
    constraints: &mut Vec<Constraint>,
    locals: &mut LocalValues,
) {
    match file.expr(expr) {
        Expr::Let { ty, value, .. } => {
            solved.must_be(expr, Type::Void).unwrap();

            if ty.is_some() {
                let ty = resolved.typ(expr);
                solved.must_be(value, ty).unwrap();
            }

            let local = resolved.local(expr);
            match locals.get_or_init(local, value) {
                Some(other) => {
                    constraints.push(Constraint::Same(value, other));
                }
                None => (),
            }

            check_expr_types(file, def, value, resolved, solved, constraints, locals);
        }
        Expr::Set { value, .. } => {
            solved.must_be(expr, Type::Void).unwrap();

            let local = resolved.local(expr);
            match locals.get_or_init(local, value) {
                Some(other) => {
                    constraints.push(Constraint::Same(value, other));
                }
                None => (),
            }

            check_expr_types(file, def, value, resolved, solved, constraints, locals);
        }
        Expr::Local(_) => {
            let local = resolved.local(expr);
            match locals.get_or_init(local, expr) {
                Some(other) => {
                    constraints.push(Constraint::Same(expr, other));
                }
                None => (),
            }
        }
        Expr::Literal(_) => {
            solved.must_be(expr, Type::S4).unwrap();
        }
        Expr::Plus(lhs, rhs) | Expr::Minus(lhs, rhs) | Expr::Times(lhs, rhs) => {
            constraints.push(Constraint::Same(lhs, rhs));
            constraints.push(Constraint::Same(lhs, expr));
            constraints.push(Constraint::Same(rhs, expr));
            check_expr_types(file, def, lhs, resolved, solved, constraints, locals);
            check_expr_types(file, def, rhs, resolved, solved, constraints, locals);
        }
        Expr::While { pred, body } => {
            solved.must_be(expr, Type::Void).unwrap();
            solved.must_be(pred, Type::S4).unwrap();

            check_expr_types(file, def, pred, resolved, solved, constraints, locals);
            check_stmts_types(file, def, body, resolved, solved, constraints, locals);
        }
        Expr::Break(value) => {
            // TODO: Resolve scopes to track that sameness
            solved.must_be(expr, Type::Void).unwrap();

            if let Some(value) = value {
                check_expr_types(file, def, value, resolved, solved, constraints, locals);
            }
        }
        Expr::Return(value) => {
            solved.must_be(expr, Type::Void).unwrap();

            if let Some(value) = value {
                let ret_ty = resolved.return_type(def.name());
                solved.must_be(value, ret_ty).unwrap();
                check_expr_types(file, def, value, resolved, solved, constraints, locals);
            }
        }
        Expr::Call { .. } => {
            todo!()
        }
    }
}

/// Order the expressions such that they take one of three shapes:
/// - (unsolved, unsolved)
/// - (solved, unsolved)
/// - (solved, solved)
fn order_by_solved(
    x: Id<Expr>,
    y: Id<Expr>,
    solved: &mut ExtendArena<Expr, Option<Type>>,
) -> (Id<Expr>, Id<Expr>) {
    if solved.get(x).is_some() {
        (x, y)
    } else {
        (y, x)
    }
}
