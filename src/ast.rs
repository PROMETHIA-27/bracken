use std::collections::HashMap;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq)]
pub struct File {
    pub def_ids: HashMap<String, usize>,
    pub defs: Vec<FnDef>,
}

impl Hash for File {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.def_ids {
            k.hash(state);
            v.hash(state);
        }
        self.defs.hash(state);
    }
}

#[cfg_attr(feature = "fern", comemo::track)]
impl File {
    pub fn def(&self, name: &str) -> &FnDef {
        &self.defs[self.def_ids[name]]
    }

    pub fn defs(&self) -> &[FnDef] {
        &self.defs
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub body: Stmts,
}

#[cfg_attr(feature = "fern", comemo::track)]
impl FnDef {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &Stmts {
        &self.body
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Stmts(pub Vec<Stmt>);

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Stmt {
    Expr(Expr),
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Expr {
    Let { name: String, value: Box<Expr> },
    Set { name: String, value: Box<Expr> },
    Local(String),
    Literal(i32),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    While { pred: Box<Expr>, body: Stmts },
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
}
