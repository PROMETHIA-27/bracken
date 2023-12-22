use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod bytecode;
pub mod error;
pub mod lower;
pub mod nameres;

lalrpop_mod!(pub parser);
