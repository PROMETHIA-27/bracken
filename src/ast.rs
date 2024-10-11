// use std::collections::BTreeMap;
// use std::fmt::Display;
// use std::hash::Hash;
// use std::ops::Range;

// use cranelift_entity::PrimaryMap;
// use thiserror::Error;

// use crate::bytecode::FunctionId;
// use crate::error::OneOf;
// use crate::parser::FileParser;
// use crate::Db;

// #[derive(Clone, Debug, Eq, Error, PartialEq)]
// pub enum ParseError {
//     #[error("invalid token encountered at {0}")]
//     InvalidToken(usize),
//     #[error("encountered unexpected EOF")]
//     UnrecognizedEof,
//     #[error("{0}..{1}: unexpected token {2} encountered; expected {3}")]
//     UnrecognizedToken(usize, usize, String, OneOf<String>),
//     #[error("unexpected extra token encountered")]
//     ExtraToken,
// }

// pub struct SourceFile {
//     pub text: String,
// }

// pub fn file_ast(db: &dyn Db, source: SourceFile) -> Result<File, ParseError> {
//     let file = FileParser::new()
//         .parse(source, db, source.text(db))
//         .map_err(|err| match err {
//             lalrpop_util::ParseError::InvalidToken { location } => {
//                 ParseError::InvalidToken(location)
//             }
//             lalrpop_util::ParseError::UnrecognizedEof { .. } => ParseError::UnrecognizedEof,
//             lalrpop_util::ParseError::UnrecognizedToken {
//                 token: (l1, t, l2),
//                 expected,
//             } => ParseError::UnrecognizedToken(
//                 l1,
//                 l2,
//                 format!("`{t}`"),
//                 OneOf::new(expected).expect("unrecognized token with no expected tokens"),
//             ),
//             lalrpop_util::ParseError::ExtraToken { .. } => ParseError::ExtraToken,
//             _ => unreachable!(),
//         })?;
//     Ok(file)
// }

// pub struct File {
//     pub source: SourceFile,

//     pub lines: Vec<usize>,

//     pub def_ids: BTreeMap<Name, FunctionId>,

//     pub defs: PrimaryMap<FunctionId, FnDef>,
//     // TODO: RIGHT NOW: add span info so I can return good errors
// }

// impl File {
//     pub fn from_defs(db: &dyn Db, source: SourceFile, defs: Vec<FnDef>) -> Self {
//         let lines = source
//             .text(db)
//             .chars()
//             .enumerate()
//             .filter_map(|(i, c)| (c == '\n').then_some(i))
//             .collect();
//         let defs: PrimaryMap<_, _> = defs.into_iter().collect();
//         let def_ids = defs.iter().map(|(i, def)| (def.name(db), i)).collect();

//         Self::new(db, source, lines, def_ids, defs)
//     }

//     pub fn offset_to_loc(&self, db: &dyn Db, offset: usize) -> Location {
//         let line = match self.lines(db).binary_search(&offset) {
//             Ok(index) | Err(index) => index - 1,
//         };
//         Location {
//             line,
//             col: offset - self.lines(db)[line],
//         }
//     }

//     pub fn def_id(&self, db: &dyn Db, def: Name) -> FunctionId {
//         self.def_ids(db)[&def]
//     }
// }

// pub struct ExprList {
//     pub exprs: Vec<Expr>,
// }

// #[derive(Clone, Copy, Debug, Eq, PartialEq)]
// pub struct Location {
//     pub line: usize,
//     pub col: usize,
// }

// impl Display for Location {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_fmt(format_args!("Line {}, Column {}", self.line, self.col))
//     }
// }

// pub struct Name {
//     pub text: String,
// }

// pub struct FnDef {
//     pub name: Name,
//     pub body: Stmts,

//     pub params: Vec<(Name, Name)>,
//     pub return_type: Option<Name>,
// }

// pub struct Stmts {
//     pub stmts: Vec<Stmt>,
// }

// #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
// pub enum Stmt {
//     Expr(Expr),
// }

// pub struct Expr {
//     pub kind: ExprKind,
//     pub span: Range<usize>,
// }

// #[derive(Clone, Debug, Eq, PartialEq)]
// pub enum ExprKind {
//     Let {
//         name: Name,
//         ty: Option<Name>,
//         value: Expr,
//     },
//     Set {
//         name: Name,
//         value: Expr,
//     },
//     Name(Name),
//     Literal(i32),
//     Plus(Expr, Expr),
//     Minus(Expr, Expr),
//     Times(Expr, Expr),
//     While {
//         pred: Expr,
//         body: Stmts,
//     },
//     Break(Option<Expr>),
//     Return(Option<Expr>),
//     Call {
//         callee: Expr,
//         params: ExprList,
//     },
//     // TODO: Parenthesized expr?
// }
