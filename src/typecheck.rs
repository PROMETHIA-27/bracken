use thiserror::Error;

use crate::arena::{ExtendArena, Id};
use crate::ast::{Expr, File};
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

#[derive(Debug, Error)]
pub enum TyCheckError {
    #[error("insufficient information to determine type of expression")]
    InsufficientInfo,
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
        self.values.resize(local + 1, None);

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
    let mut constraints = vec![];
    let mut locals = LocalValues::new();
    let mut errors = Errors::new();

    for expr in file.exprs().ids() {
        match file.expr(expr) {
            Expr::Let { ty, value, .. } => {
                solved.set(expr, Some(Type::Void));

                if ty.is_some() {
                    let ty = resolved.typ(expr);
                    solved.set(value, Some(ty));
                }

                let local = resolved.local(expr);
                match locals.get_or_init(local, value) {
                    Some(other) => {
                        constraints.push(Constraint::Same(value, other));
                    }
                    None => (),
                }
            }
            Expr::Set { value, .. } => {
                solved.set(expr, Some(Type::Void));

                let local = resolved.local(expr);
                match locals.get_or_init(local, value) {
                    Some(other) => {
                        constraints.push(Constraint::Same(value, other));
                    }
                    None => (),
                }
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
                solved.set(expr, Some(Type::S4));
            }
            Expr::Plus(lhs, rhs) | Expr::Minus(lhs, rhs) | Expr::Times(lhs, rhs) => {
                constraints.push(Constraint::Same(lhs, rhs));
            }
            Expr::While { pred, .. } => {
                solved.set(expr, Some(Type::Void));
                solved.set(pred, Some(Type::S4));
            }
            Expr::Break(_) => {
                // TODO: Resolve scopes to track that sameness
                solved.set(expr, Some(Type::Void));
            }
            Expr::Return(val) => {
                solved.set(expr, Some(Type::Void));
                // TODO: Read from function ret ty
                if let Some(val) = val {}
            }
        }
    }

    let mut i = 0;
    let mut changed = false;
    let mut exit = false;
    while !exit {
        match constraints[i] {
            Constraint::Same(x, y) => {
                let (x, y) = order_by_solved(x, y, &mut solved);
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
                    solved.set(y, Some(xty));
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
        }
    }

    for constraint in constraints {
        match constraint {
            Constraint::Same(_x, _y) => {
                errors.push(TyCheckError::InsufficientInfo);
            }
        }
    }

    if errors.is_empty() {
        Ok(SolvedTypes {
            tys: solved.map(Option::unwrap),
        })
    } else {
        Err(errors)
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
