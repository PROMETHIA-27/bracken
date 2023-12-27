use std::collections::HashMap;

use crate::arena::Id;
use crate::ast::{Expr, File, FnDef, Stmt, Stmts};
use crate::bytecode::Type;

#[derive(Clone, Debug)]
pub struct Resolved {
    locals: HashMap<Id<Expr>, u32>,
    types: HashMap<Id<Expr>, Type>,
}

impl Resolved {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn local(&self, expr: Id<Expr>) -> u32 {
        self.locals[&expr]
    }

    pub fn local_len(&self) -> usize {
        self.locals.len()
    }

    pub fn typ(&self, expr: Id<Expr>) -> Type {
        self.types[&expr]
    }
}

pub fn resolve_names(file: &File) -> Resolved {
    let mut resolved = Resolved::new();

    resolve_file(file, &mut resolved);

    resolved
}

pub fn resolve_file(file: &File, resolved: &mut Resolved) {
    let mut stack = ScopeStack::new();
    stack.push(Scope::top_level_scope());

    for def in file.defs() {
        resolve_func(file, def, &mut stack, resolved);
    }
}

fn resolve_func<'f>(
    file: &'f File,
    def: &'f FnDef,
    stack: &mut ScopeStack<'f>,
    resolved: &mut Resolved,
) {
    resolve_stmts(file, file.stmts(def.body()), stack, resolved);
}

fn resolve_stmts<'f>(
    file: &'f File,
    stmts: &'f Stmts,
    scopes: &mut ScopeStack<'f>,
    resolved: &mut Resolved,
) {
    for stmt in stmts.stmts().iter().copied() {
        match stmt {
            Stmt::Expr(expr) => resolve_expr(file, expr, scopes, resolved),
        }
    }
}

fn resolve_expr<'f>(
    file: &'f File,
    expr: Id<Expr>,
    scopes: &mut ScopeStack<'f>,
    resolved: &mut Resolved,
) {
    match file.expr(expr) {
        Expr::Let { name, ty, value } => {
            resolve_expr(file, value, scopes, resolved);

            let local = scopes.top_function_mut().kind_mut().alloc_local();
            scopes.add_local(file.str(name), local);
            resolved.locals.insert(expr, local);

            if let Some(ty) = ty {
                let ty = scopes.ty(file.str(ty)).expect("unbound name");
                resolved.types.insert(expr, ty);
            }
        }
        Expr::Set { name, value } => {
            resolve_expr(file, value, scopes, resolved);
            let local = scopes.local(file.str(name)).expect("unbound name");
            resolved.locals.insert(expr, local);
        }
        Expr::Local(name) => {
            let local = scopes.local(file.str(name)).expect("unbound name");
            resolved.locals.insert(expr, local);
        }
        Expr::Plus(lhs, rhs) | Expr::Minus(lhs, rhs) | Expr::Times(lhs, rhs) => {
            resolve_expr(file, lhs, scopes, resolved);
            resolve_expr(file, rhs, scopes, resolved);
        }
        Expr::While { pred, body } => {
            resolve_expr(file, pred, scopes, resolved);

            scopes.push(Scope::loop_());
            resolve_stmts(file, file.stmts(body), scopes, resolved);
            scopes.pop();
        }
        Expr::Break(val) | Expr::Return(val) => {
            if let Some(val) = val {
                resolve_expr(file, val, scopes, resolved);
            }
        }
        Expr::Literal(_) => (),
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ScopeItem {
    Local(u32),
    Type(Type),
}

impl ScopeItem {
    pub fn local(self) -> u32 {
        match self {
            ScopeItem::Local(local) => local,
            _ => panic!(),
        }
    }

    pub fn ty(self) -> Type {
        match self {
            ScopeItem::Type(ty) => ty,
            _ => panic!(),
        }
    }
}

struct Scope<'f> {
    items: HashMap<&'f str, ScopeItem>,
    kind: ScopeKind,
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

impl<'f> Scope<'f> {
    fn new(kind: ScopeKind) -> Self {
        Self {
            items: HashMap::default(),
            kind,
        }
    }

    pub fn function() -> Self {
        Self::new(ScopeKind::Function { locals: 0 })
    }

    pub fn loop_() -> Self {
        Self::new(ScopeKind::Loop)
    }

    pub fn top_level_scope() -> Self {
        let mut scope = Self::new(ScopeKind::File);
        scope.items.insert("S4", ScopeItem::Type(Type::S4));
        scope.items.insert("F4", ScopeItem::Type(Type::F4));
        scope
    }

    pub fn kind_mut(&mut self) -> &mut ScopeKind {
        &mut self.kind
    }

    pub fn items(&self) -> &HashMap<&str, ScopeItem> {
        &self.items
    }
}

struct ScopeStack<'f> {
    scopes: Vec<Scope<'f>>,
    functions: Vec<usize>,
    // TODO: HashMap<&str, SmallVec<[ScopeItem; 1]>>,
}

impl<'f> Default for ScopeStack<'f> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'f> ScopeStack<'f> {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            functions: vec![],
        }
    }

    pub fn local(&self, name: &str) -> Option<u32> {
        for scope in self.scopes.iter().rev() {
            if let Some(local) = scope.items.get(name) {
                return Some(local.local());
            }
        }
        None
    }

    pub fn ty(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.items.get(name) {
                return Some(ty.ty());
            }
        }
        None
    }

    pub fn add_local(&mut self, name: &'f str, local: u32) {
        self.scopes
            .last_mut()
            .unwrap()
            .items
            .insert(name, ScopeItem::Local(local));
    }

    pub fn add_type(&mut self, name: &'f str, ty: Type) {
        self.scopes
            .last_mut()
            .unwrap()
            .items
            .insert(name, ScopeItem::Type(ty));
    }

    pub fn top(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn top_function_mut(&mut self) -> &mut Scope<'f> {
        &mut self.scopes[*self.functions.last().unwrap()]
    }

    pub fn push(&mut self, scope: Scope<'f>) {
        match scope.kind {
            ScopeKind::Function { .. } => self.functions.push(self.scopes.len()),
            _ => (),
        }

        self.scopes.push(scope);
    }

    pub fn pop(&mut self) {
        match self.scopes.pop().expect("popped too many scopes").kind {
            ScopeKind::Function { .. } => _ = self.functions.pop().unwrap(),
            _ => (),
        }
    }
}
