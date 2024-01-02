use std::collections::HashMap;
use std::ops::Range;

use thiserror::Error;

use crate::ast::{Expr, ExprKind, File, FnDef, Location, Stmt, Stmts};
use crate::bytecode::Type;
use crate::error::Errors;
use crate::nameres::Resolved;
use crate::Db;

pub struct SolvedTypes {
    tys: HashMap<Expr, Type>,
}

impl SolvedTypes {
    pub fn get(&self, expr: Expr) -> Type {
        *self
            .tys
            .get(&expr)
            .expect("failed to solve all types but did not detect it")
    }
}

trait SolvedMapExt {
    fn must_be(&mut self, id: Expr, value: Type) -> Result<(), ()>;
}

impl SolvedMapExt for HashMap<Expr, Type> {
    fn must_be(&mut self, id: Expr, value: Type) -> Result<(), ()> {
        match self.get(&id) {
            Some(old) => {
                if old != &value {
                    Err(())
                } else {
                    Ok(())
                }
            }
            None => {
                self.insert(id, value);
                Ok(())
            }
        }
    }
}

#[derive(Clone, Copy)]
pub enum Constraint {
    Same(Expr, Expr),
}

#[derive(Clone, Debug, Error)]
pub enum TyCheckError {
    // TODO: use SourceText instead and DebugWithDb
    #[error("insufficient information to determine type of expression `{}` at {}", &.0[.1.clone()], .2)]
    InsufficientInfo(String, Range<usize>, Location),
    #[error("type mismatch")]
    TypeMismatch,
}

struct LocalValues {
    values: Vec<Option<Expr>>,
}

impl LocalValues {
    fn new() -> Self {
        Self { values: vec![] }
    }

    fn get_or_init(&mut self, local: u32, value: Expr) -> Option<Expr> {
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

pub fn check_types(
    db: &dyn Db,
    file: &File,
    resolved: &Resolved,
) -> Result<SolvedTypes, Errors<TyCheckError>> {
    let mut solved = HashMap::new();
    let mut errors = Errors::new();

    for def in file.defs(db) {
        check_fn_types(db, file, def, resolved, &mut solved, &mut errors);
    }

    if errors.is_empty() {
        Ok(SolvedTypes { tys: solved })
    } else {
        Err(errors)
    }
}

fn check_fn_types(
    db: &dyn Db,
    file: &File,
    def: &FnDef,
    resolved: &Resolved,
    solved: &mut HashMap<Expr, Type>,
    errors: &mut Errors<TyCheckError>,
) {
    let mut constraints = vec![];
    let mut locals = LocalValues::new();

    check_stmts_types(
        db,
        file,
        def,
        def.body,
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
                let (xs, ys) = (solved.get(&x).is_some(), solved.get(&y).is_some());
                if xs && ys {
                    // both solved
                    let (xty, yty) = (solved.get(&x).unwrap(), solved.get(&y).unwrap());
                    if xty != yty {
                        errors.push(TyCheckError::TypeMismatch);
                    }
                    constraints.swap_remove(i);
                    changed = true;
                } else if xs && !ys {
                    // x solved, y unsolved
                    let xty = solved.get(&x).unwrap();
                    solved.must_be(y, *xty).unwrap();
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
                    file.source(db).text(db).clone(),
                    x.span(db),
                    file.offset_to_loc(db, x.span(db).start),
                ));
                errors.push(TyCheckError::InsufficientInfo(
                    file.source(db).text(db).clone(),
                    y.span(db),
                    file.offset_to_loc(db, y.span(db).start),
                ));
            }
        }
    }
}

fn check_stmts_types(
    db: &dyn Db,
    file: &File,
    def: &FnDef,
    stmts: Stmts,
    resolved: &Resolved,
    solved: &mut HashMap<Expr, Type>,
    constraints: &mut Vec<Constraint>,
    locals: &mut LocalValues,
) {
    for stmt in stmts.stmts(db) {
        match stmt {
            &Stmt::Expr(expr) => {
                check_expr_types(db, file, def, expr, resolved, solved, constraints, locals)
            }
        }
    }
}

fn check_expr_types(
    db: &dyn Db,
    file: &File,
    def: &FnDef,
    expr: Expr,
    resolved: &Resolved,
    solved: &mut HashMap<Expr, Type>,
    constraints: &mut Vec<Constraint>,
    locals: &mut LocalValues,
) {
    match expr.kind(db) {
        ExprKind::Let { ty, value, .. } => {
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

            check_expr_types(db, file, def, value, resolved, solved, constraints, locals);
        }
        ExprKind::Set { value, .. } => {
            solved.must_be(expr, Type::Void).unwrap();

            let local = resolved.local(expr);
            match locals.get_or_init(local, value) {
                Some(other) => {
                    constraints.push(Constraint::Same(value, other));
                }
                None => (),
            }

            check_expr_types(db, file, def, value, resolved, solved, constraints, locals);
        }
        ExprKind::Name(_) => {
            let local = resolved.local(expr);
            match locals.get_or_init(local, expr) {
                Some(other) => {
                    constraints.push(Constraint::Same(expr, other));
                }
                None => (),
            }
        }
        ExprKind::Literal(_) => {
            solved.must_be(expr, Type::S4).unwrap();
        }
        ExprKind::Plus(lhs, rhs) | ExprKind::Minus(lhs, rhs) | ExprKind::Times(lhs, rhs) => {
            constraints.push(Constraint::Same(lhs, rhs));
            constraints.push(Constraint::Same(lhs, expr));
            constraints.push(Constraint::Same(rhs, expr));
            check_expr_types(db, file, def, lhs, resolved, solved, constraints, locals);
            check_expr_types(db, file, def, rhs, resolved, solved, constraints, locals);
        }
        ExprKind::While { pred, body } => {
            solved.must_be(expr, Type::Void).unwrap();
            solved.must_be(pred, Type::S4).unwrap();

            check_expr_types(db, file, def, pred, resolved, solved, constraints, locals);
            check_stmts_types(db, file, def, body, resolved, solved, constraints, locals);
        }
        ExprKind::Break(value) => {
            // TODO: Resolve scopes to track that sameness
            solved.must_be(expr, Type::Void).unwrap();

            if let Some(value) = value {
                check_expr_types(db, file, def, value, resolved, solved, constraints, locals);
            }
        }
        ExprKind::Return(value) => {
            solved.must_be(expr, Type::Void).unwrap();

            if let Some(value) = value {
                let ret_ty = resolved.return_type(def.name);
                solved.must_be(value, ret_ty).unwrap();
                check_expr_types(db, file, def, value, resolved, solved, constraints, locals);
            }
        }
        ExprKind::Call { callee, params } => {
            // TODO: type checking the callee gonna be funky
            solved.must_be(callee, Type::Function).unwrap();
            let func = resolved.callee(expr);
            let param_tys = resolved.params(func);

            let params = params.exprs(db);
            assert_eq!(params.len(), param_tys.len());

            for (&param_ty, &param) in param_tys.iter().zip(params) {
                solved.must_be(param, param_ty).unwrap();
            }
        }
    }
}

/// Order the expressions such that they take one of three shapes:
/// - (unsolved, unsolved)
/// - (solved, unsolved)
/// - (solved, solved)
fn order_by_solved(x: Expr, y: Expr, solved: &mut HashMap<Expr, Type>) -> (Expr, Expr) {
    if solved.get(&x).is_some() {
        (x, y)
    } else {
        (y, x)
    }
}
