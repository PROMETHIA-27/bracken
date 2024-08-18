use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Range;

use cranelift_entity::PrimaryMap;
use thiserror::Error;

use crate::bytecode::FunctionId;
use crate::error::OneOf;
use crate::parser::FileParser;
use crate::Db;

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum ParseError {
    #[error("invalid token encountered at {0}")]
    InvalidToken(usize),
    #[error("encountered unexpected EOF")]
    UnrecognizedEof,
    #[error("{0}..{1}: unexpected token {2} encountered; expected {3}")]
    UnrecognizedToken(usize, usize, String, OneOf<String>),
    #[error("unexpected extra token encountered")]
    ExtraToken,
}

#[salsa::input]
pub struct SourceFile {
    #[return_ref]
    pub text: String,
}

#[salsa::tracked]
pub fn file_ast(db: &dyn Db, source: SourceFile) -> Result<File, ParseError> {
    let file = FileParser::new()
        .parse(source, db, source.text(db))
        .map_err(|err| match err {
            lalrpop_util::ParseError::InvalidToken { location } => {
                ParseError::InvalidToken(location)
            }
            lalrpop_util::ParseError::UnrecognizedEof { .. } => ParseError::UnrecognizedEof,
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (l1, t, l2),
                expected,
            } => ParseError::UnrecognizedToken(
                l1,
                l2,
                format!("`{t}`"),
                OneOf::new(expected).expect("unrecognized token with no expected tokens"),
            ),
            lalrpop_util::ParseError::ExtraToken { .. } => ParseError::ExtraToken,
            _ => unreachable!(),
        })?;
    Ok(file)
}

#[salsa::tracked]
pub struct File {
    pub source: SourceFile,
    #[return_ref]
    pub lines: Vec<usize>,
    #[return_ref]
    pub def_ids: BTreeMap<Name, FunctionId>,
    #[return_ref]
    pub defs: PrimaryMap<FunctionId, FnDef>,
    // TODO: RIGHT NOW: add span info so I can return good errors
}

impl File {
    pub fn from_defs(db: &dyn Db, source: SourceFile, defs: Vec<FnDef>) -> Self {
        let lines = source
            .text(db)
            .chars()
            .enumerate()
            .filter_map(|(i, c)| (c == '\n').then_some(i))
            .collect();
        let defs: PrimaryMap<_, _> = defs.into_iter().collect();
        let def_ids = defs.iter().map(|(i, def)| (def.name(db), i)).collect();

        Self::new(db, source, lines, def_ids, defs)
    }

    pub fn offset_to_loc(&self, db: &dyn Db, offset: usize) -> Location {
        let line = match self.lines(db).binary_search(&offset) {
            Ok(index) | Err(index) => index - 1,
        };
        Location {
            line,
            col: offset - self.lines(db)[line],
        }
    }

    pub fn def_id(&self, db: &dyn Db, def: Name) -> FunctionId {
        self.def_ids(db)[&def]
    }
}

#[salsa::tracked]
pub struct ExprList {
    #[return_ref]
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Line {}, Column {}", self.line, self.col))
    }
}

#[salsa::interned]
pub struct Name {
    #[return_ref]
    pub text: String,
}

#[salsa::tracked]
pub struct FnDef {
    pub name: Name,
    pub body: Stmts,
    #[return_ref]
    pub params: Vec<(Name, Name)>,
    pub return_type: Option<Name>,
}

#[salsa::tracked]
pub struct Stmts {
    #[return_ref]
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
}

#[salsa::tracked]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Range<usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprKind {
    Let {
        name: Name,
        ty: Option<Name>,
        value: Expr,
    },
    Set {
        name: Name,
        value: Expr,
    },
    Name(Name),
    Literal(i32),
    Plus(Expr, Expr),
    Minus(Expr, Expr),
    Times(Expr, Expr),
    While {
        pred: Expr,
        body: Stmts,
    },
    Break(Option<Expr>),
    Return(Option<Expr>),
    Call {
        callee: Expr,
        params: ExprList,
    },
    // TODO: Parenthesized expr?
}
