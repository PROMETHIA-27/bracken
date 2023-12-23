use std::collections::BTreeMap;
use std::hash::Hash;

use comemo::track;

use crate::arena::{Arena, Id, IndexedArena, IntoArena};

#[derive(Clone)]
pub struct Ast<'ast> {
    exprs: Arena<Expr<'ast>>,
    files: Vec<File<'ast>>,
}

impl<'ast> Hash for Ast<'ast> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.files.hash(state);
    }
}

impl<'ast> PartialEq for Ast<'ast> {
    fn eq(&self, other: &Self) -> bool {
        self.files == other.files
    }
}

impl<'ast> Drop for Ast<'ast> {
    fn drop(&mut self) {
        drop(std::mem::take(&mut self.files));
        _ = self.exprs;
    }
}

impl<'ast> Ast<'ast> {
    pub fn new(indexed: IndexedArena<IExpr>, files: Vec<IFile>) -> Self {
        let arena = Arena::from_indexed(indexed);
        Ast {
            files: files
                .into_iter()
                .map(|file| file.into_ref(&arena))
                .collect(),
            exprs: arena,
        }
    }

    pub fn files(&self) -> &[File<'ast>] {
        &self.files
    }
}

pub struct IFile {
    def_ids: BTreeMap<String, usize>,
    defs: Vec<IFnDef>,
}

impl IFile {
    pub fn new(defs: Vec<IFnDef>) -> Self {
        Self {
            def_ids: defs
                .iter()
                .enumerate()
                .map(|(i, def)| (def.name.clone(), i))
                .collect(),
            defs,
        }
    }

    fn into_ref<'ast>(self, arena: &Arena<Expr<'ast>>) -> File<'ast> {
        File {
            def_ids: self.def_ids,
            defs: self
                .defs
                .into_iter()
                // SAFETY:
                // - The AST is immutable and private after creation so nothing will be dropped until it is dropped
                //   whole, and its drop impl drops the arena last
                .map(|def| def.into_ref(unsafe { arena.slice() }))
                .collect(),
        }
    }
}

#[derive(Clone, Hash, PartialEq)]
pub struct File<'ast> {
    def_ids: BTreeMap<String, usize>,
    defs: Vec<FnDef<'ast>>,
}

#[track]
impl<'ast> File<'ast> {
    pub fn def(&self, name: &str) -> &FnDef {
        &self.defs[self.def_ids[name]]
    }

    pub fn defs(&self) -> &[FnDef] {
        &self.defs
    }
}

pub struct IFnDef {
    name: String,
    body: IStmts,
}

impl IFnDef {
    pub fn new(name: String, body: IStmts) -> Self {
        IFnDef { name, body }
    }

    fn into_ref<'ast>(self, children: &'ast [Expr<'ast>]) -> FnDef<'ast> {
        FnDef {
            name: self.name,
            body: self.body.into_ref(children),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct FnDef<'ast> {
    name: String,
    body: Stmts<'ast>,
}

#[track]
impl<'ast> FnDef<'ast> {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &Stmts {
        &self.body
    }
}

pub struct IStmts(Vec<IStmt>);

impl IStmts {
    pub fn new(stmts: Vec<IStmt>) -> Self {
        Self(stmts)
    }

    fn into_ref<'ast>(self, children: &'ast [Expr]) -> Stmts<'ast> {
        Stmts(
            self.0
                .into_iter()
                .map(|stmt| stmt.into_ref(children))
                .collect(),
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Stmts<'ast>(Vec<Stmt<'ast>>);

impl<'ast> Stmts<'ast> {
    pub fn stmts(&self) -> &[Stmt<'ast>] {
        &self.0
    }
}

pub enum IStmt {
    Expr(Id<IExpr>),
}

impl IStmt {
    fn into_ref<'ast>(self, children: &'ast [Expr]) -> Stmt<'ast> {
        match self {
            IStmt::Expr(exp) => Stmt::Expr(&children[exp.index()]),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Stmt<'ast> {
    Expr(&'ast Expr<'ast>),
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

pub enum IExpr {
    Let {
        name: String,
        ty: Option<String>,
        value: Id<IExpr>,
    },
    Set {
        name: String,
        value: Id<IExpr>,
    },
    Local(String),
    Literal(i32),
    Plus(Id<IExpr>, Id<IExpr>),
    Minus(Id<IExpr>, Id<IExpr>),
    Times(Id<IExpr>, Id<IExpr>),
    While {
        pred: Id<IExpr>,
        body: IStmts,
    },
    Break(Option<Id<IExpr>>),
    Return(Option<Id<IExpr>>),
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Expr<'ast> {
    Let {
        name: String,
        ty: Option<String>,
        value: &'ast Expr<'ast>,
    },
    Set {
        name: String,
        value: &'ast Expr<'ast>,
    },
    Local(String),
    Literal(i32),
    Plus(&'ast Expr<'ast>, &'ast Expr<'ast>),
    Minus(&'ast Expr<'ast>, &'ast Expr<'ast>),
    Times(&'ast Expr<'ast>, &'ast Expr<'ast>),
    While {
        pred: &'ast Expr<'ast>,
        body: Stmts<'ast>,
    },
    Break(Option<&'ast Expr<'ast>>),
    Return(Option<&'ast Expr<'ast>>),
}

impl<'ast> IntoArena<'ast> for Expr<'ast> {
    type Indexed = IExpr;

    fn to_ref(indexed: Self::Indexed, children: &'ast [Self]) -> Self {
        match indexed {
            IExpr::Let { name, ty, value } => Expr::Let {
                name,
                ty,
                value: &children[value.index()],
            },
            IExpr::Set { name, value } => Expr::Set {
                name,
                value: &children[value.index()],
            },
            IExpr::Local(loc) => Expr::Local(loc),
            IExpr::Literal(lit) => Expr::Literal(lit),
            IExpr::Plus(l, r) => Expr::Plus(&children[l.index()], &children[r.index()]),
            IExpr::Minus(l, r) => Expr::Minus(&children[l.index()], &children[r.index()]),
            IExpr::Times(l, r) => Expr::Times(&children[l.index()], &children[r.index()]),
            IExpr::While { pred, body } => Expr::While {
                pred: &children[pred.index()],
                body: body.into_ref(children),
            },
            IExpr::Break(value) => Expr::Break(value.map(|v| &children[v.index()])),
            IExpr::Return(value) => Expr::Return(value.map(|v| &children[v.index()])),
        }
    }
}
