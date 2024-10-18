use prolangkit::db::{ArenaStored, Db, DebugWithContext, Id};
use prolangkit::Spanned;

pub struct FunctionDef {
    pub name: Spanned<Id<str>>,
    pub params: Spanned<Vec<Spanned<Param>>>,
    pub return_ty: Option<Spanned<Id<str>>>,
    pub body: Id<Vec<Id<Expr>>>,
}

impl DebugWithContext<Db> for FunctionDef {
    fn fmt(&self, ctx: &Db, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionDef")
            .field("name", &self.name.with(ctx))
            .field("params", &self.params.with(ctx))
            .field("return_ty", &self.return_ty.with(ctx))
            .field("body", &self.body.with(ctx))
            .finish()
    }
}

pub struct Param {
    pub name: Spanned<Id<str>>,
    pub ty: Spanned<Id<str>>,
}

impl DebugWithContext<Db> for Param {
    fn fmt(&self, ctx: &Db, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionDef")
            .field("name", &self.name.with(ctx))
            .field("ty", &self.ty.with(ctx))
            .finish()
    }
}

pub type Expr = Spanned<ExprInner>;

impl ArenaStored for ExprInner {}
impl ArenaStored for FunctionDef {}

pub enum ExprInner {
    Ident(Id<str>),
    Literal(Id<str>),
    Return(Option<Id<Expr>>),
    While {
        cond: Id<Expr>,
        body: Id<Vec<Id<Expr>>>,
    },
    Break,
    FnCall {
        callee: Id<Expr>,
        args: Id<Vec<Id<Expr>>>,
    },
    Plus {
        lhs: Id<Expr>,
        rhs: Id<Expr>,
    },
    Minus {
        lhs: Id<Expr>,
        rhs: Id<Expr>,
    },
    Mul {
        lhs: Id<Expr>,
        rhs: Id<Expr>,
    },
    VariableDecl {
        name: Id<str>,
        ty: Option<Id<str>>,
        value: Id<Expr>,
    },
    VariableAssign {
        name: Id<str>,
        value: Id<Expr>,
    },
    Error,
}

impl DebugWithContext<Db> for ExprInner {
    fn fmt(&self, ctx: &Db, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprInner::Ident(id) => f.debug_tuple("Ident").field(&id.with(ctx)).finish(),
            ExprInner::Literal(id) => f.debug_tuple("Literal").field(&id.with(ctx)).finish(),
            ExprInner::Return(id) => f.debug_tuple("Return").field(&id.with(ctx)).finish(),
            ExprInner::While { cond, body } => f
                .debug_struct("While")
                .field("cond", &cond.with(ctx))
                .field("body", &body.with(ctx))
                .finish(),
            ExprInner::Break => f.debug_struct("Break").finish(),
            ExprInner::FnCall { callee, args } => f
                .debug_struct("FnCall")
                .field("callee", &callee.with(ctx))
                .field("args", &args.with(ctx))
                .finish(),
            ExprInner::Plus { lhs, rhs } => f
                .debug_struct("Plus")
                .field("lhs", &lhs.with(ctx))
                .field("rhs", &rhs.with(ctx))
                .finish(),
            ExprInner::Minus { lhs, rhs } => f
                .debug_struct("Minus")
                .field("lhs", &lhs.with(ctx))
                .field("rhs", &rhs.with(ctx))
                .finish(),
            ExprInner::Mul { lhs, rhs } => f
                .debug_struct("Mul")
                .field("lhs", &lhs.with(ctx))
                .field("rhs", &rhs.with(ctx))
                .finish(),
            ExprInner::VariableDecl { name, ty, value } => f
                .debug_struct("VariableDecl")
                .field("name", &name.with(ctx))
                .field("ty", &ty.with(ctx))
                .field("value", &value.with(ctx))
                .finish(),
            ExprInner::VariableAssign { name, value } => f
                .debug_struct("VariableAssign")
                .field("name", &name.with(ctx))
                .field("value", &value.with(ctx))
                .finish(),
            ExprInner::Error => f.debug_struct("Error").finish(),
        }
    }
}
