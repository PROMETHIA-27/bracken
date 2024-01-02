use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, File, FnDef, Name, Stmt, Stmts};
use crate::bytecode::Type;
use crate::Db;

#[salsa::tracked]
pub struct Resolved {
    #[return_ref]
    data: ResolvedData,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ResolvedData {
    locals: HashMap<Expr, u32>,
    local_count: HashMap<Name, usize>,
    types: HashMap<Expr, Type>,
    params: HashMap<Name, Vec<Type>>,
    return_types: HashMap<Name, Type>,
    callees: HashMap<Expr, Name>,
}

impl Resolved {
    pub fn local(self, db: &dyn Db, expr: Expr) -> u32 {
        self.data(db).locals[&expr]
    }

    pub fn local_len(self, db: &dyn Db, func: Name) -> usize {
        self.data(db).local_count[&func]
    }

    pub fn typ(self, db: &dyn Db, expr: Expr) -> Type {
        self.data(db).types[&expr]
    }

    pub fn params_of(self, db: &dyn Db, func: Name) -> &[Type] {
        &self.data(db).params[&func]
    }

    pub fn return_type(self, db: &dyn Db, func: Name) -> Type {
        self.data(db).return_types[&func]
    }

    pub fn callee(self, db: &dyn Db, expr: Expr) -> Name {
        self.data(db).callees[&expr]
    }
}

#[salsa::tracked]
pub fn resolve_names(db: &dyn Db, file: File) -> Resolved {
    let mut resolved = ResolvedData::default();

    resolve_file(db, file, &mut resolved);

    Resolved::new(db, resolved)
}

pub fn resolve_file(db: &dyn Db, file: File, resolved: &mut ResolvedData) {
    let mut stack = ScopeStack::new();
    stack.push_top_level_scope(db);

    for def in file.defs(db) {
        gather_func(db, def, &mut stack);
        resolved.local_count.insert(def.name(db), 0);
    }

    for def in file.defs(db) {
        resolve_func(db, file, def, &mut stack, resolved);
    }
}

fn gather_func(db: &dyn Db, def: &FnDef, stack: &mut ScopeStack) {
    stack.add_function(def.name(db));
}

fn resolve_func(
    db: &dyn Db,
    file: File,
    def: &FnDef,
    stack: &mut ScopeStack,
    resolved: &mut ResolvedData,
) {
    let mut params = vec![];
    for &(_, ty) in &def.params(db) {
        let ty = stack.ty(ty).unwrap();
        params.push(ty);
    }
    resolved.params.insert(def.name(db), params);

    let ret_ty = def
        .return_type(db)
        .map(|ty| stack.ty(ty).unwrap())
        .unwrap_or(Type::Void);
    resolved.return_types.insert(def.name(db), ret_ty);

    stack.push(ScopeKind::Function { locals: 0 });
    resolve_stmts(db, file, def, def.body(db), stack, resolved);
    stack.pop();
}

fn resolve_stmts(
    db: &dyn Db,
    file: File,
    def: &FnDef,
    stmts: Stmts,
    scopes: &mut ScopeStack,
    resolved: &mut ResolvedData,
) {
    for stmt in stmts.stmts(db).iter().copied() {
        match stmt {
            Stmt::Expr(expr) => resolve_expr(db, file, def, expr, scopes, resolved),
        }
    }
}

fn resolve_expr(
    db: &dyn Db,
    file: File,
    def: &FnDef,
    expr: Expr,
    scopes: &mut ScopeStack,
    resolved: &mut ResolvedData,
) {
    match expr.kind(db) {
        ExprKind::Let { name, ty, value } => {
            resolve_expr(db, file, def, value, scopes, resolved);

            let local = scopes.top_function_mut().alloc_local();
            scopes.add_local(name, local);
            *resolved.local_count.get_mut(&def.name(db)).unwrap() += 1;
            resolved.locals.insert(expr, local);

            if let Some(ty) = ty {
                let ty = scopes.ty(ty).expect("unbound name");
                resolved.types.insert(expr, ty);
            }
        }
        ExprKind::Set { name, value } => {
            resolve_expr(db, file, def, value, scopes, resolved);
            let local = scopes.local(name).expect("unbound name");
            resolved.locals.insert(expr, local);
        }
        ExprKind::Name(name) => match *scopes.item(name).expect("unbound name") {
            ScopeItem::Local(local) => {
                resolved.locals.insert(expr, local);
            }
            ScopeItem::Type(_) => panic!("type in expression location"),
            ScopeItem::Function => (),
        },
        ExprKind::Plus(lhs, rhs) | ExprKind::Minus(lhs, rhs) | ExprKind::Times(lhs, rhs) => {
            resolve_expr(db, file, def, lhs, scopes, resolved);
            resolve_expr(db, file, def, rhs, scopes, resolved);
        }
        ExprKind::While { pred, body } => {
            resolve_expr(db, file, def, pred, scopes, resolved);

            scopes.push(ScopeKind::Loop);
            resolve_stmts(db, file, def, body, scopes, resolved);
            scopes.pop();
        }
        ExprKind::Break(val) | ExprKind::Return(val) => {
            if let Some(val) = val {
                resolve_expr(db, file, def, val, scopes, resolved);
            }
        }
        ExprKind::Literal(_) => (),
        ExprKind::Call { callee, params } => {
            // TODO: calls can only be names right now and that breaks to resolve
            // resolve_expr(file, def, callee, scopes, resolved);

            for &param in params.exprs(db) {
                resolve_expr(db, file, def, param, scopes, resolved);
            }

            match callee.kind(db) {
                ExprKind::Name(name) => {
                    scopes.function(name).expect("unbound name");
                    resolved.callees.insert(expr, name);
                }
                _ => todo!(),
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ScopeItem {
    Local(u32),
    Type(Type),
    Function,
}

impl ScopeItem {
    pub fn local(&self) -> u32 {
        match self {
            ScopeItem::Local(local) => *local,
            _ => panic!(),
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            ScopeItem::Type(ty) => *ty,
            _ => panic!(),
        }
    }

    pub fn function(&self) {
        match self {
            ScopeItem::Function => (),
            _ => panic!(),
        }
    }
}

enum ScopeKind {
    Function { locals: u32 },
    Loop,
    File,
}

impl ScopeKind {
    pub fn alloc_local(&mut self) -> u32 {
        match self {
            ScopeKind::Function { locals } => {
                let index = *locals;
                *locals += 1;
                index
            }
            _ => panic!("not a function scope"),
        }
    }
}

struct ScopeStack {
    scopes: Vec<(ScopeKind, usize)>,
    items: Vec<(Name, ScopeItem)>,
    functions: Vec<usize>,
    // TODO: test perf of HashMap<&str, SmallVec<[ScopeItem; 1]>>,
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            items: vec![],
            functions: vec![],
        }
    }

    pub fn push_top_level_scope(&mut self, db: &dyn Db) {
        self.push(ScopeKind::File);
        self.add_type(Name::new(db, "S4".to_string()), Type::S4);
        self.add_type(Name::new(db, "F4".to_string()), Type::F4);
    }

    pub fn item(&self, name: Name) -> Option<&ScopeItem> {
        self.items
            .iter()
            .rev()
            .find_map(|(n, item)| (*n == name).then_some(item))
    }

    pub fn local(&self, name: Name) -> Option<u32> {
        self.item(name).map(ScopeItem::local)
    }

    pub fn ty(&self, name: Name) -> Option<Type> {
        self.item(name).map(ScopeItem::ty)
    }

    pub fn function(&self, name: Name) -> Option<()> {
        self.item(name).map(ScopeItem::function)
    }

    pub fn add_local(&mut self, name: Name, local: u32) {
        self.items.push((name, ScopeItem::Local(local)));
    }

    pub fn add_type(&mut self, name: Name, ty: Type) {
        self.items.push((name, ScopeItem::Type(ty)));
    }

    pub fn add_function(&mut self, name: Name) {
        self.items.push((name, ScopeItem::Function));
    }

    pub fn top_function_mut(&mut self) -> &mut ScopeKind {
        &mut self.scopes[*self.functions.last().unwrap()].0
    }

    pub fn push(&mut self, scope: ScopeKind) {
        match scope {
            ScopeKind::Function { .. } => self.functions.push(self.scopes.len()),
            _ => (),
        }

        self.scopes.push((scope, self.items.len()));
    }

    pub fn pop(&mut self) {
        let (kind, height) = self.scopes.pop().expect("popped too many scopes");

        match kind {
            ScopeKind::Function { .. } => _ = self.functions.pop().unwrap(),
            _ => (),
        }

        self.items.truncate(height);
    }
}
