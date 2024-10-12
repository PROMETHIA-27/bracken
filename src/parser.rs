use crate::db::Db;
use logos::{Lexer, Logos};
use pomelo::pomelo;

pub use parser::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Loc {
    start: u32,
    end: u32,
}

impl Loc {
    pub fn start(&self) -> usize {
        self.start.try_into().unwrap()
    }
    
    pub fn end(&self) -> usize {
        self.end.try_into().unwrap()
    }

    pub fn span(&self, other: Loc) -> Loc {
        Loc {
            start: self.start,
            end: other.end,
        }
    }
}

pomelo! {
    %include {
        use crate::parser::Loc;
        use crate::db::{Id, Db};
        use logos::{Logos, Lexer};
        
        fn location<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> Loc {
            Loc {
                start: lexer.span().start.try_into().unwrap(),
                end: lexer.span().end.try_into().unwrap(),
            }
        }

        fn string<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> (Loc, &'src str) {
            (location(lexer), lexer.slice())
        }
    };

    // Tokens
    %token
    #[derive(Logos, Clone, Debug, PartialEq)]
    #[logos(skip r"[ \n\t\f]+")]
    pub enum Token<'src> {};

    %type #[regex("([a-zA-Z_][a-zA-Z0-9_]*)|([0-9]+[a-zA-Z][a-zA-Z0-9]*)", string)] Ident &'src str;
    %type #[regex("[0-9]+", string)] Literal &'src str;

    // Keywords
    %type #[token("function", location)] Function;
    %type #[token("end", location)] End;
    %type #[token("let", location)] Let;
    %type #[token("return", location)] Return;
    %type #[token("while", location)] While;
    %type #[token("break", location)] Break;

    // Symbols
    %type #[token("(", location)] LParen;
    %type #[token(")", location)] RParen;
    %type #[token("{", location)] LBrace;
    %type #[token("}", location)] RBrace;
    %type #[token("=", location)] Equal;
    %type #[token("+", location)] Plus;
    %type #[token("-", location)] Minus;
    %type #[token("*", location)] Mul;
    %type #[token(",", location)] Comma;
    %type #[token(":", location)] Colon;
    %type #[token("->", location)] RArrow;

    %extra_argument Db;

    %extra_token Loc;

    %nonassoc Equal;
    %left Plus Minus;
    %left Mul;

    file ::= fn_defs?;

    fn_defs ::= fn_def;
    fn_defs ::= fn_defs fn_def;
    fn_def ::= Function Ident LParen params? RParen return_type? stmts? End;
    params ::= param;
    params ::= params Comma param;
    param ::= Ident Colon Ident;
    return_type ::= RArrow Ident;

    stmts ::= expr;
    stmts ::= stmts expr;

    exprs ::= expr;
    exprs ::= exprs Comma expr;

    expr ::= Let Ident typeAscription? Equal expr;
    typeAscription ::= Colon Ident;
    expr ::= Ident Equal expr;

    expr ::= expr Plus expr;
    expr ::= expr Minus expr;

    expr ::= expr Mul expr;

    expr ::= atom;

    atom ::= return_;
    atom ::= while_;
    atom ::= break_;
    atom ::= fn_call;
    atom ::= Literal;
    atom ::= Ident;
    atom ::= LBrace expr RBrace;

    return_ ::= Return LParen expr? RParen;
    while_ ::= While expr stmts? End;
    break_ ::= Break;
    fn_call ::= atom LParen exprs? RParen;
}

pub fn test() {
    let text = include_str!("../in.br");

    let tokens = Token::lexer(text);

    let mut parser = Parser::new(Db::default());

    for tok in tokens {
        println!("Lexed {:?}", tok.clone().unwrap());
        parser.parse(tok.unwrap()).unwrap();
    }

    parser.end_of_input().unwrap();
}