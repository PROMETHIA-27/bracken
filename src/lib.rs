use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod bytecode;
pub mod error;
pub mod lower;
pub mod nameres;
pub mod typecheck;

lalrpop_mod!(pub parser);

#[salsa::jar(db = Db)]
pub struct Jar(
    ast::SourceFile,
    ast::File,
    ast::Expr,
    ast::file_ast,
    ast::Stmts,
    ast::Name,
    ast::ExprList,
    nameres::Resolved,
    nameres::resolve_names,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> {}

#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {}

impl salsa::ParallelDatabase for Database {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Database {
            storage: self.storage.snapshot(),
        })
    }
}
