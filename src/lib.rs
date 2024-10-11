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
    %left Plus Minus;
    %left Mul;

    file ::= fnDefs;

    fnDefs ::= fnDef;
    fnDefs ::= fnDefs Comma fnDef;
    fnDef ::= Function Ident LParen params RParen returnType? stmts End;
    params ::= param;
    params ::= params Comma param;
    param ::= Ident Colon Ident;
    returnType ::= RArrow Ident;

    stmts ::= expr;
    stmts ::= stmts Semicolon expr;

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
    while_ ::= While expr stmts End;
    break_ ::= Break;
    fnCall ::= factor LParen exprs RParen;
}

fn test() {
    // let mut parser = Parser::new();
    // for tok in [Token::Ident("Bruh".into())] {
    //     parser.parse(tok).unwrap();
    // }
    // let data = parser.end_of_input().unwrap();
}
