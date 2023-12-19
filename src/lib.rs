#[cfg(feature = "fern")]
use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod bytecode;

#[cfg(feature = "fern")]
lalrpop_mod!(pub parser);
