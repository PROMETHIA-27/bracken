use bincode::{DefaultOptions, Options};
use bracken::ast::{Expr, File, FnDef, Stmt, Stmts};
use bracken::bytecode::{Func, LabelIndex, Module, Opcode, SSAIndex};
use bracken::parser::FileParser;
use bracken::{Errors, OneOf};
use comemo::{memoize, track, Track, Tracked};
use thiserror::Error;

fn main() -> anyhow::Result<()> {
    let file = std::env::args().nth(1).unwrap();
    let module = compile(std::fs::read_to_string(file)?)?;
    println!("{module:?}");
    std::fs::write("out.brm", module).unwrap();
    Ok(())
}

#[memoize]
fn compute_ast(source: String) -> Result<Ast, ParseError> {
    Ok(Ast(FileParser::new().parse(&source).map_err(
        |err| match err {
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
        },
    )?))
}

#[derive(Clone, Debug, Error)]
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

#[derive(Clone)]
pub struct Ast(File);

#[track]
impl Ast {
    pub fn file(&self) -> &File {
        &self.0
    }
}

#[memoize]
fn compute_bytecode(source: String) -> Result<Module, Errors<ParseError>> {
    let ast = compute_ast(source)?;
    let funcs = ast
        .file()
        .defs()
        .iter()
        .map(|def| compile_func(def.track()))
        .collect::<Vec<Func>>();
    Ok(Module { funcs })
}

#[memoize]
fn compile_func(def: Tracked<FnDef>) -> Func {
    let mut func = Func {
        name: def.name().to_string(),
        opcodes: vec![],
        labels: vec![],
        label_pool: vec![],
    };
    compile_stmts(def.body(), &mut func, &mut vec![]);
    func
}

fn compile_stmts(stmts: &Stmts, func: &mut Func, scopes: &mut Vec<Scope>) {
    for stmt in &stmts.0 {
        match stmt {
            Stmt::Expr(expr) => _ = compile_expr(expr, func, scopes),
        }
    }
}

struct Scope {
    end: LabelIndex,
}

fn compile_expr(expr: &Expr, func: &mut Func, scopes: &mut Vec<Scope>) -> SSAIndex {
    match expr {
        &Expr::Literal(constant) => func.push_op(Opcode::LiteralS4(constant)),
        Expr::Plus(lhs, rhs) => {
            let lhs = compile_expr(lhs, func, scopes);
            let rhs = compile_expr(rhs, func, scopes);
            func.push_op(Opcode::Add(lhs, rhs))
        }
        Expr::Times(lhs, rhs) => {
            let lhs = compile_expr(lhs, func, scopes);
            let rhs = compile_expr(rhs, func, scopes);
            func.push_op(Opcode::Mult(lhs, rhs))
        }
        Expr::While { pred, body } => {
            let after_loop = func.add_label();
            let pred = compile_expr(pred, func, scopes);
            let fallthrough = func.add_label();
            let targets = func.push_label_slice(&[fallthrough]);
            func.push_op(Opcode::Branch { pred, default: after_loop, targets });
            func.set_label(fallthrough, SSAIndex(func.ops().len() as u32));
            scopes.push(Scope { end: after_loop });
            compile_stmts(body, func, scopes);
            scopes.pop();
            func.set_label(after_loop, SSAIndex(func.ops().len() as u32));
            SSAIndex::UNSET
        }
        Expr::Break(val) => {
            // TODO: Non-empty breaks
            _ = val;
            // TODO: Good error on break without scope
            let label = scopes.last().unwrap().end;
            func.push_op(Opcode::Jump(label))
        }
        Expr::Return(val) => {
            // TODO: Empty returns
            let val = val.as_ref().unwrap();
            let val = compile_expr(val, func, scopes);
            func.push_op(Opcode::Ret(val))
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Serialize(#[from] SerError),
}

#[derive(Clone, Debug, Error)]
pub enum SerError {
    #[error("serialization failed")]
    Failed,
}

#[memoize]
pub fn compile(source: String) -> Result<Vec<u8>, Errors<Error>> {
    let module = compute_bytecode(source)
        .map_err(|errs| errs.into_iter().map(Error::from).collect::<Vec<_>>())?;
    Ok(DefaultOptions::new()
        .with_varint_encoding()
        .allow_trailing_bytes()
        .serialize(&module)
        .map_err(|_| SerError::Failed)
        .map_err(Error::Serialize)?)
}

#[cfg(test)]
mod tests {
    use bracken::ast::{Expr, FnDef, Stmt, Stmts};
    use bracken::parser::FnDefParser;

    #[test]
    fn return_0() {
        assert_eq!(
            FnDefParser::new().parse("function main() 0 end").unwrap(),
            FnDef {
                name: "main".to_string(),
                body: Stmts(vec![Stmt::Expr(Expr::Literal(0)),]),
            }
        );
    }
}
