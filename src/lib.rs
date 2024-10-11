use parser::{Parser, Token};
// use lalrpop_util::lalrpop_mod;
use pomelo::pomelo;

pub mod ast;
pub mod bytecode;
pub mod error;
pub mod lower;
pub mod nameres;
pub mod typecheck;

// lalrpop_mod!(pub parser);

pomelo! {
    %token #[derive(Clone, Copy, Debug)] pub enum Token {};

    %left Plus Minus;
    %left Mul;

    file ::= fnDefs?;

    fnDefs ::= fnDef;
    fnDefs ::= fnDefs fnDef;
    fnDef ::= Function Ident LParen params? RParen returnType? stmts? End;
    params ::= param;
    params ::= params Comma param;
    param ::= Ident Colon Ident;
    returnType ::= RArrow Ident;

    stmts ::= expr;
    stmts ::= stmts expr;

    exprs ::= expr;
    exprs ::= exprs Comma expr;

    expr ::= assignExpr;

    assignExpr ::= Let Ident typeAscription? Equal assignExpr;
    typeAscription ::= Colon Ident;
    assignExpr ::= Ident Equal assignExpr;
    assignExpr ::= valueExpr;

    valueExpr ::= valueExpr Plus valueExpr;
    valueExpr ::= valueExpr Minus valueExpr;
    valueExpr ::= term;

    term ::= term Mul term;
    term ::= factor;

    factor ::= return_;
    factor ::= while_;
    factor ::= break_;
    factor ::= fnCall;
    factor ::= Literal;
    factor ::= Ident;
    factor ::= LBrace expr RBrace;

    return_ ::= Return LParen expr? RParen;
    while_ ::= While expr stmts? End;
    break_ ::= Break;
    fnCall ::= factor LParen exprs? RParen;
}

pub fn test() {
    use Token::*;
    let mut parser = Parser::new();
    for tok in [
        Function,
        Ident,
        LParen,
        RParen,
        RArrow,
        Ident,
        Let,
        Ident,
        Equal,
        Literal,
        Let,
        Ident,
        Equal,
        Literal,
        While,
        Ident,
        Minus,
        Literal,
        Ident,
        Equal,
        Ident,
        Plus,
        Literal,
        Ident,
        Equal,
        Ident,
        Mul,
        Literal,
        End,
        Return,
        LParen,
        Ident,
        Plus,
        Ident,
        LParen,
        RParen,
        RParen,
        End,
        Function,
        Ident,
        LParen,
        RParen,
        RArrow,
        Ident,
        Return,
        LParen,
        Literal,
        RParen,
        End,
    ] {
        parser.parse(tok).unwrap();
        println!("Parsed {tok:?}");
    }
    let data = parser.end_of_input().unwrap();
}
