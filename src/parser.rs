use crate::ast::{Expr, FunctionDef};
use logos::Logos;
use pomelo::pomelo;

pub use parser::*;
use prolangkit::db::{Db, DebugWithContext, Id};
use prolangkit::Spanned;

pomelo! {
    %include {
        use crate::ast::{Expr, ExprInner, FunctionDef, Param};
        use prolangkit::db::{Id, Db};
        use prolangkit::{Loc, Spanned};
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

        fn binary_op(
            lhs: Id<Expr>,
            rhs: Id<Expr>,
            db: &mut Db,
            f: impl FnOnce(Id<Expr>, Id<Expr>) -> ExprInner
        ) -> Id<Expr> {
            let l = db.get(lhs).loc;
            let r = db.get(rhs).loc;
            db.insert(l.span(r).with(f(lhs, rhs)))
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

    // AST Nodes
    %type file Vec<Id<Spanned<FunctionDef>>>;
    %type fn_defs Vec<Id<Spanned<FunctionDef>>>;
    %type fn_def Id<Spanned<FunctionDef>>;
    %type params Spanned<Vec<Spanned<Param>>>;
    %type param Spanned<Param>;
    %type return_type (Loc, Id<str>);

    %type expr Id<Expr>;
    %type type_ascription (Loc, Id<str>);
    %type atom Id<Expr>;
    %type exprs Id<Vec<Id<Expr>>>;
    %type stmts Id<Vec<Id<Expr>>>;
    %type ident (Loc, Id<str>);
    %type literal (Loc, Id<str>);

    %extra_argument Db;

    %extra_token Loc;

    %nonassoc Equal;
    %left Plus Minus;
    %left Mul;

    // Start point
    file ::= fn_defs?(defs) { defs.unwrap_or_else(Vec::new) };

    // Functions
    fn_defs ::= fn_def(def) { vec![def] };
    fn_defs ::= fn_defs(mut rest) fn_def(def) {
        rest.push(def);
        rest
    };
    fn_def ::= Function(l) ident(name) LParen(lparen) params?(params) RParen(rparen) return_type?(return_ty) stmts?(body) End(r) {
        let name = name.0.with(name.1);
        let params = params.unwrap_or_else(|| lparen.span(rparen).with(vec![]));
        let return_ty = return_ty.map(|ty| ty.0.with(ty.1));
        let body = body.unwrap_or(extra.empty_vec());
        extra.insert(l.span(r).with(FunctionDef {
            name,
            params,
            return_ty,
            body
        }))
    };
    params ::= param(param) { param.loc.with(vec![param]) };
    params ::= params(mut rest) Comma param(param) {
        rest.loc.end = param.loc.end;
        rest.value.push(param);
        rest
    };
    param ::= ident((l, name)) Colon ident((r, ty)) {
        l.span(r).with(Param {
            name: l.with(name),
            ty: r.with(ty)
        })
    };
    return_type ::= RArrow(l) ident((r, name)) { (l.span(r), name) };

    // Non-separated exprs
    stmts ::= expr(expr) {
        extra.insert(vec![expr])
    };
    stmts ::= stmts(rest) expr(next) {
        extra.get_mut(rest).push(next);
        rest
    };

    // Comma separated exprs
    exprs ::= expr(expr) {
        extra.insert(vec![expr])
    };
    exprs ::= exprs(rest) Comma expr(next) {
        extra.get_mut(rest).push(next);
        rest
    };

    // Assignment
    expr ::= Let(l) ident((_, name)) type_ascription?(ty) Equal expr(value) {
        let r = extra.get(value).loc;
        extra.insert(l.span(r).with(ExprInner::VariableDecl { name, ty: ty.map(|ty| ty.1), value }))
    };
    type_ascription ::= Colon(l) ident((r, name)) { (l.span(r), name) };
    expr ::= ident((l, name)) Equal expr(value) {
        let r = extra.get(value).loc;
        extra.insert(l.span(r).with(ExprInner::VariableAssign { name, value }))
    };

    // Addition, subtraction
    expr ::= expr(lhs) Plus expr(rhs) { binary_op(lhs, rhs, extra, |lhs, rhs| ExprInner::Plus { lhs, rhs }) };
    expr ::= expr(lhs) Minus expr(rhs) { binary_op(lhs, rhs, extra, |lhs, rhs| ExprInner::Minus { lhs, rhs }) };

    // Multiplication, division, remainder
    expr ::= expr(lhs) Mul expr(rhs) { binary_op(lhs, rhs, extra, |lhs, rhs| ExprInner::Mul { lhs, rhs }) };

    // Atoms
    expr ::= atom;

    // literal
    atom ::= literal((loc, lit)) {
        extra.insert(loc.with(ExprInner::Literal(lit)))
    };
    // ident
    atom ::= ident((loc, ident)) { extra.insert(loc.with(ExprInner::Ident(ident))) };
    // parenthesized
    atom ::= LBrace expr(expr) RBrace { expr };
    // return
    atom ::= Return(l) LParen expr?(rval) RParen(r) {
        extra.insert(l.span(r).with(ExprInner::Return(rval)))
    };
    // while
    atom ::= While(l) expr(cond) stmts?(body) End(r) {
        extra.insert(
            l.span(r)
            .with(ExprInner::While {
                cond,
                body: body.unwrap_or(extra.empty_vec())
            })
        )
    };
    // break
    atom ::= Break(loc) { extra.insert(loc.with(ExprInner::Break)) };
    // function call
    atom ::= atom(callee) LParen exprs?(args) RParen(r) {
        let l = extra.get(callee).loc;
        extra.insert(
            l.span(r)
            .with(ExprInner::FnCall {
                callee,
                args: args.unwrap_or(extra.empty_vec())
            })
        )
    };

    ident ::= Ident((loc, ident)) { (loc, extra.insert(ident)) }
    literal ::= Literal((loc, lit)) { (loc, extra.insert(lit)) }
}

pub fn test() {
    let text = include_str!("../in.br");

    let tokens = Token::lexer(text);

    let mut db = Db::default()
        .register::<str>()
        .register::<Expr>()
        .register::<Vec<Id<Expr>>>()
        .register::<Spanned<FunctionDef>>();

    db.insert::<Vec<Id<Expr>>>(vec![]);

    let mut parser = Parser::new(db);

    for tok in tokens {
        println!("Lexed {:?}", tok.clone().unwrap());
        parser.parse(tok.unwrap()).unwrap();
    }

    let (parsed, db) = parser.end_of_input().unwrap();

    println!("{:#?}", parsed.with(&db));
}
