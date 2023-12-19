use bincode::{DefaultOptions, Options};
use bracken::ast::{Expr, File, FnDef};
use bracken::bytecode::{BeltOffset, Func, Module, Opcode};
use bracken::parser::FileParser;
use comemo::{memoize, track, Track, Tracked};
use thiserror::Error;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let module = compile(std::fs::read_to_string(file).unwrap()).unwrap();
    println!("{module:?}");
    std::fs::write("out.brm", module).unwrap();
}

#[memoize]
fn compute_ast(source: String) -> Result<Ast, ParseError> {
    Ok(Ast(FileParser::new().parse(&source).map_err(
        |err| match err {
            lalrpop_util::ParseError::InvalidToken { .. } => ParseError::InvalidToken,
            lalrpop_util::ParseError::UnrecognizedEof { .. } => ParseError::UnrecognizedEof,
            lalrpop_util::ParseError::UnrecognizedToken { .. } => ParseError::UnrecognizedToken,
            lalrpop_util::ParseError::ExtraToken { .. } => ParseError::ExtraToken,
            _ => unreachable!(),
        },
    )?))
}

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("invalid token encountered")]
    InvalidToken,
    #[error("encountered unexpected EOF")]
    UnrecognizedEof,
    #[error("unexpected token encountered")]
    UnrecognizedToken,
    #[error("unexpected extra token encountered")]
    ExtraToken,
}

#[derive(Clone)]
pub struct Ast(File);

#[track]
impl Ast {
    pub fn file(&self) -> &File {
        &self.0
    }
}

#[derive(Clone, Debug, Error)]
pub enum BytecodeError {}

#[memoize]
fn compute_bytecode(ast: Tracked<Ast>) -> Result<Module, Vec<BytecodeError>> {
    let funcs = ast
        .file()
        .defs()
        .iter()
        .map(|def| compile_func(def.track()))
        .collect::<Vec<Result<Func, BytecodeError>>>();
    let errors = funcs
        .iter()
        .filter(|&res| res.is_err())
        .cloned()
        .map(Result::unwrap_err)
        .collect::<Vec<BytecodeError>>();
    if errors.is_empty() {
        Ok(Module {
            funcs: funcs.into_iter().map(Result::unwrap).collect::<Vec<Func>>(),
        })
    } else {
        Err(errors)
    }
}

#[memoize]
fn compile_func(def: Tracked<FnDef>) -> Result<Func, BytecodeError> {
    let mut func = Func {
        name: def.name().to_string(),
        opcodes: vec![],
    };
    let ret_val = compile_expr(def.body(), &mut func);
    func.push_op(Opcode::Ret(ret_val));
    Ok(func)
}

fn compile_expr(expr: &Expr, func: &mut Func) -> BeltOffset {
    match expr {
        &Expr::Literal(constant) => func.push_op(Opcode::LiteralS4(constant)),
        Expr::Plus(lhs, rhs) => {
            let lhs = compile_expr(lhs, func);
            let rhs = compile_expr(rhs, func);
            func.push_op(Opcode::Add(lhs, rhs))
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Bytecode(#[from] BytecodeError),
    #[error(transparent)]
    Serialize(#[from] SerError),
}

#[derive(Clone, Debug, Error)]
pub enum SerError {
    #[error("serialization failed")]
    Failed,
}

#[memoize]
pub fn compile(source: String) -> Result<Vec<u8>, Vec<Error>> {
    let ast = compute_ast(source).map_err(|err| vec![err.into()])?;
    let module = compute_bytecode(ast.track())
        .map_err(|errs| errs.into_iter().map(Error::from).collect::<Vec<_>>())?;
    DefaultOptions::new()
        .with_varint_encoding()
        .allow_trailing_bytes()
        .serialize(&module)
        .map_err(|_| vec![Error::Serialize(SerError::Failed)])
}

#[cfg(test)]
mod tests {
    use bracken::ast::{Expr, FnDef};
    use bracken::parser::FnDefParser;

    #[test]
    fn return_0() {
        assert_eq!(
            FnDefParser::new().parse("function main() 0 end").unwrap(),
            FnDef {
                name: "main".to_string(),
                body: Expr::Literal(0)
            }
        );
    }
}
