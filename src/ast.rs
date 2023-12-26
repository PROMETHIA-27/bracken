use std::collections::BTreeMap;
use std::hash::Hash;

use comemo::track;

use crate::arena::{Arena, Id, Interner};

#[derive(Clone)]
pub struct File {
    exprs: Arena<Expr>,
    // TODO: Move this to a higher level
    strings: Interner<String>,
    stmts: Arena<Stmts>,
    def_ids: BTreeMap<Id<String>, usize>,
    defs: Vec<FnDef>,
}

impl Drop for File {
    fn drop(&mut self) {
        self.defs.clear();
    }
}

impl Hash for File {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_ids.hash(state);
        self.defs.hash(state);
    }
}

impl PartialEq for File {
    fn eq(&self, other: &Self) -> bool {
        self.def_ids == other.def_ids && self.defs == other.defs
    }
}

impl File {
    pub fn new(
        exprs: Arena<Expr>,
        strings: Interner<String>,
        stmts: Arena<Stmts>,
        defs: Vec<FnDef>,
    ) -> Self {
        Self {
            def_ids: defs
                .iter()
                .enumerate()
                .map(|(i, def)| (def.name(), i))
                .collect(),
            exprs,
            strings,
            stmts,
            defs,
        }
    }
}

#[track]
impl File {
    pub fn expr(&self, expr: Id<Expr>) -> Expr {
        *self.exprs.get(expr)
    }

    pub fn str(&self, string: Id<String>) -> &str {
        self.strings.get(string)
    }

    pub fn stmts(&self, stmts: Id<Stmts>) -> &Stmts {
        self.stmts.get(stmts)
    }

    pub fn def(&self, name: Id<String>) -> &FnDef {
        &self.defs[self.def_ids[&name]]
    }

    pub fn defs(&self) -> &[FnDef] {
        &self.defs
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct FnDef {
    name: Id<String>,
    body: Id<Stmts>,
}

impl FnDef {
    pub fn new(name: Id<String>, body: Id<Stmts>) -> Self {
        FnDef { name, body }
    }
}

#[track]
impl FnDef {
    pub fn name(&self) -> Id<String> {
        self.name
    }

    pub fn body(&self) -> Id<Stmts> {
        self.body
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Stmts(Vec<Stmt>);

impl Stmts {
    pub fn new(stmts: Vec<Stmt>) -> Stmts {
        Stmts(stmts)
    }
}

#[track]
impl Stmts {
    pub fn stmts(&self) -> &[Stmt] {
        &self.0
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum Stmt {
    Expr(Id<Expr>),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub struct ExprId(u32);

impl ExprId {
    pub fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    pub fn index(&self) -> usize {
        self.0.try_into().unwrap()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum Expr {
    Let {
        name: Id<String>,
        ty: Option<Id<String>>,
        value: Id<Expr>,
    },
    Set {
        name: Id<String>,
        value: Id<Expr>,
    },
    Local(Id<String>),
    Literal(i32),
    Plus(Id<Expr>, Id<Expr>),
    Minus(Id<Expr>, Id<Expr>),
    Times(Id<Expr>, Id<Expr>),
    While {
        pred: Id<Expr>,
        body: Id<Stmts>,
    },
    Break(Option<Id<Expr>>),
    Return(Option<Id<Expr>>),
}
