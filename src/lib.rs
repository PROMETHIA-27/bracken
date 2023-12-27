use lalrpop_util::lalrpop_mod;

pub mod arena;
pub mod ast;
pub mod bytecode;
pub mod error;
pub mod lower;
pub mod nameres;
pub mod typecheck;

lalrpop_mod!(pub parser);
