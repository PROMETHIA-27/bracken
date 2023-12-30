use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Range;
use std::sync::Arc;

use crate::arena::{Arena, ExtendArena, Id, Interner};

#[derive(Clone)]
pub struct File {
    source: Arc<String>,
    lines: Vec<usize>,
    exprs: Arena<Expr>,
    exprlists: Arena<Vec<Id<Expr>>>,
    spans: ExtendArena<Expr, (usize, usize)>,
    // TODO: Move this to a higher level
    strings: Interner<String>,
    stmts: Arena<Stmts>,
    def_ids: BTreeMap<Id<String>, usize>,
    defs: Vec<FnDef>,
    // TODO: RIGHT NOW: add span info so I can return good errors
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

#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Line {}, Column {}", self.line, self.col))
    }
}

impl File {
    pub fn new(
        source: Arc<String>,
        exprs: Arena<Expr>,
        exprlists: Arena<Vec<Id<Expr>>>,
        spans: ExtendArena<Expr, (usize, usize)>,
        mut strings: Interner<String>,
        stmts: Arena<Stmts>,
        defs: Vec<FnDef>,
    ) -> Self {
        Self::intern_builtin_strings(&mut strings);

        Self {
            lines: source
                .chars()
                .enumerate()
                .filter_map(|(i, c)| (c == '\n').then_some(i))
                .collect(),
            source,
            def_ids: defs
                .iter()
                .enumerate()
                .map(|(i, def)| (def.name(), i))
                .collect(),
            exprs,
            exprlists,
            spans,
            strings,
            stmts,
            defs,
        }
    }

    fn intern_builtin_strings(strings: &mut Interner<String>) {
        strings.add("S4".into());
        strings.add("F4".into());
    }

    pub fn source(&self) -> &Arc<String> {
        &self.source
    }

    pub fn exprs(&self) -> &Arena<Expr> {
        &self.exprs
    }

    pub fn expr(&self, expr: Id<Expr>) -> Expr {
        *self.exprs.get(expr)
    }

    pub fn spans(&self) -> &ExtendArena<Expr, (usize, usize)> {
        &self.spans
    }

    pub fn span(&self, expr: Id<Expr>) -> Range<usize> {
        let (s, e) = *self.spans.get_no_resize(expr).unwrap();
        s..e
    }

    pub fn offset_to_loc(&self, offset: usize) -> Location {
        let line = match self.lines.binary_search(&offset) {
            Ok(index) | Err(index) => index - 1,
        };
        Location {
            line,
            col: offset - self.lines[line],
        }
    }

    pub fn str(&self, string: Id<String>) -> &str {
        self.strings.get(string)
    }

    pub fn str_id(&self, string: &str) -> Id<String> {
        self.strings.get_id(string).unwrap()
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
    params: Vec<(Id<String>, Id<String>)>,
    ret_ty: Option<Id<String>>,
}

impl FnDef {
    pub fn new(
        name: Id<String>,
        body: Id<Stmts>,
        params: Vec<(Id<String>, Id<String>)>,
        ret_ty: Option<Id<String>>,
    ) -> Self {
        FnDef {
            name,
            body,
            params,
            ret_ty,
        }
    }

    pub fn name(&self) -> Id<String> {
        self.name
    }

    pub fn body(&self) -> Id<Stmts> {
        self.body
    }

    pub fn params(&self) -> &[(Id<String>, Id<String>)] {
        &self.params
    }

    pub fn return_type(&self) -> Option<Id<String>> {
        self.ret_ty
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Stmts(Vec<Stmt>);

impl Stmts {
    pub fn new(stmts: Vec<Stmt>) -> Stmts {
        Stmts(stmts)
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.0
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum Stmt {
    Expr(Id<Expr>),
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
    Call {
        callee: Id<Expr>,
        params: Id<Vec<Id<Expr>>>,
    },
}
