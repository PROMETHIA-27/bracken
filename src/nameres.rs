use std::collections::HashMap;

use crate::arena::Id;
use crate::ast::{Expr, File, FnDef, Stmt, Stmts};
use crate::bytecode::Type;

#[derive(Clone, Debug)]
pub struct Resolved {
    locals: HashMap<Id<Expr>, u32>,
    local_count: HashMap<Id<String>, usize>,
    types: HashMap<Id<Expr>, Type>,
    params: HashMap<Id<String>, Vec<Type>>,
    return_types: HashMap<Id<String>, Type>,
    callee: HashMap<Id<Expr>, Id<String>>,
}

impl Resolved {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            local_count: HashMap::new(),
            types: HashMap::new(),
            params: HashMap::new(),
            return_types: HashMap::new(),
            callee: HashMap::new(),
        }
    }

    pub fn local(&self, expr: Id<Expr>) -> u32 {
        self.locals[&expr]
    }

    pub fn local_len(&self, func: Id<String>) -> usize {
        self.local_count[&func]
    }

    pub fn typ(&self, expr: Id<Expr>) -> Type {
        self.types[&expr]
    }

    pub fn params(&self, func: Id<String>) -> &[Type] {
        &self.params[&func]
    }

    pub fn return_type(&self, func: Id<String>) -> Type {
        self.return_types[&func]
    }

    pub fn callee(&self, call: Id<Expr>) -> Id<String> {
        self.callee[&call]
    }
}

pub fn resolve_names(file: &File) -> Resolved {
    let mut resolved = Resolved::new();

    resolve_file(file, &mut resolved);

    resolved
}

pub fn resolve_file(file: &File, resolved: &mut Resolved) {
    let mut stack = ScopeStack::new();
    stack.push(Scope::top_level_scope(file));

    for def in file.defs() {
        gather_func(def, &mut stack);
        resolved.local_count.insert(def.name(), 0);
    }

    for def in file.defs() {
        resolve_func(file, def, &mut stack, resolved);
    }
}

fn gather_func(def: &FnDef, stack: &mut ScopeStack) {
    stack.add_function(def.name());
}

fn resolve_func(file: &File, def: &FnDef, stack: &mut ScopeStack, resolved: &mut Resolved) {
    let mut params = vec![];
    for &(name, ty) in def.params() {
        let ty = stack.ty(ty).unwrap();
        params.push(ty);
    }
    resolved.params.insert(def.name(), params);

    let ret_ty = def
        .return_type()
        .map(|ty| stack.ty(ty).unwrap())
        .unwrap_or(Type::Void);
    resolved.return_types.insert(def.name(), ret_ty);

    stack.push(Scope::function());
    resolve_stmts(file, def, file.stmts(def.body()), stack, resolved);
    stack.pop();
}

fn resolve_stmts(
    file: &File,
    def: &FnDef,
    stmts: &Stmts,
    scopes: &mut ScopeStack,
    resolved: &mut Resolved,
) {
    for stmt in stmts.stmts().iter().copied() {
        match stmt {
            Stmt::Expr(expr) => resolve_expr(file, def, expr, scopes, resolved),
        }
    }
}

fn resolve_expr(
    file: &File,
    def: &FnDef,
    expr: Id<Expr>,
    scopes: &mut ScopeStack,
    resolved: &mut Resolved,
) {
    match file.expr(expr) {
        Expr::Let { name, ty, value } => {
            resolve_expr(file, def, value, scopes, resolved);

            let local = scopes.top_function_mut().kind_mut().alloc_local();
            scopes.add_local(name, local);
            *resolved.local_count.get_mut(&def.name()).unwrap() += 1;
            resolved.locals.insert(expr, local);

            if let Some(ty) = ty {
                let ty = scopes.ty(ty).expect("unbound name");
                resolved.types.insert(expr, ty);
            }
        }
        Expr::Set { name, value } => {
            resolve_expr(file, def, value, scopes, resolved);
            let local = scopes.local(name).expect("unbound name");
            resolved.locals.insert(expr, local);
        }
        Expr::Name(name) => {
            match *scopes.item(name).expect("unbound name") {
                ScopeItem::Local(local) => {
                    resolved.locals.insert(expr, local);
                },
                ScopeItem::Type(_) => panic!("type in expression location"),
                ScopeItem::Function => (),
            }
        }
        Expr::Plus(lhs, rhs) | Expr::Minus(lhs, rhs) | Expr::Times(lhs, rhs) => {
            resolve_expr(file, def, lhs, scopes, resolved);
            resolve_expr(file, def, rhs, scopes, resolved);
        }
        Expr::While { pred, body } => {
            resolve_expr(file, def, pred, scopes, resolved);

            scopes.push(Scope::loop_());
            resolve_stmts(file, def, file.stmts(body), scopes, resolved);
            scopes.pop();
        }
        Expr::Break(val) | Expr::Return(val) => {
            if let Some(val) = val {
                resolve_expr(file, def, val, scopes, resolved);
            }
        }
        Expr::Literal(_) => (),
        Expr::Call { callee, params } => {
            // TODO: calls can only be names right now and that breaks to resolve
            // resolve_expr(file, def, callee, scopes, resolved);

            for &param in file.exprlist(params) {
                resolve_expr(file, def, param, scopes, resolved);
            }

            match file.expr(callee) {
                Expr::Name(name) => {
                    let func = scopes.function(name).expect("unbound name");
                    resolved.callee.insert(expr, func);
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

struct Scope {
    items: HashMap<Id<String>, ScopeItem>,
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

impl Scope {
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

    pub fn top_level_scope(file: &File) -> Self {
        let mut scope = Self::new(ScopeKind::File);
        scope
            .items
            .insert(file.str_id("S4"), ScopeItem::Type(Type::S4));
        scope
            .items
            .insert(file.str_id("F4"), ScopeItem::Type(Type::F4));
        scope
    }

    pub fn kind_mut(&mut self) -> &mut ScopeKind {
        &mut self.kind
    }

    pub fn items(&self) -> &HashMap<Id<String>, ScopeItem> {
        &self.items
    }
}

struct ScopeStack {
    scopes: Vec<Scope>,
    functions: Vec<usize>,
    // TODO: HashMap<&str, SmallVec<[ScopeItem; 1]>>,
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
            functions: vec![],
        }
    }

    pub fn item(&self, name: Id<String>) -> Option<&ScopeItem> {
        for scope in self.scopes.iter().rev() {
            if let Some(local) = scope.items.get(&name) {
                return Some(local);
            }
        }
        None
    }

    pub fn local(&self, name: Id<String>) -> Option<u32> {
        self.item(name).map(ScopeItem::local)
    }

    pub fn ty(&self, name: Id<String>) -> Option<Type> {
        self.item(name).map(ScopeItem::ty)
    }

    pub fn function(&self, name: Id<String>) -> Option<Id<String>> {
        self.item(name).map(ScopeItem::function).map(|_| name)
    }

    pub fn add_local(&mut self, name: Id<String>, local: u32) {
        self.scopes
            .last_mut()
            .unwrap()
            .items
            .insert(name, ScopeItem::Local(local));
    }

    pub fn add_type(&mut self, name: Id<String>, ty: Type) {
        self.scopes
            .last_mut()
            .unwrap()
            .items
            .insert(name, ScopeItem::Type(ty));
    }

    pub fn add_function(&mut self, name: Id<String>) {
        self.scopes
            .last_mut()
            .unwrap()
            .items
            .insert(name, ScopeItem::Function);
    }

    pub fn top(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn top_function_mut(&mut self) -> &mut Scope {
        &mut self.scopes[*self.functions.last().unwrap()]
    }

    pub fn push(&mut self, scope: Scope) {
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
