// use bracken::lower::compile;
// use bracken::Database;

// fn main() -> anyhow::Result<()> {
//     let file = std::env::args().nth(1).unwrap();
//     let db = Database::default();
//     let module = compile(&db, std::fs::read_to_string(file)?)?;
//     println!("{module:?}");
//     std::fs::write("out.brm", module).unwrap();
//     Ok(())
// }

fn main() {
    bracken::parser::test();
}