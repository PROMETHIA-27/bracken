use bracken::lower::compile;

fn main() -> anyhow::Result<()> {
    let file = std::env::args().nth(1).unwrap();
    let module = compile(std::fs::read_to_string(file)?)?;
    println!("{module:?}");
    std::fs::write("out.brm", module).unwrap();
    Ok(())
}
