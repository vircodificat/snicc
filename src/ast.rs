use bstr::BString;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub decls: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Fn(FuncDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl {
    pub name: BString,
    pub params: Vec<BString>,
    pub stmts: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Variable { name: BString },
    VariableInit { name: BString, expr: Box<Expr> },
    Assign { name: BString, expr: Box<Expr> },
    Print { expr: Box<Expr> },
    Exit,
    Ret(Box<Expr>),
    Expr(Box<Expr>),
    Loop(Vec<Statement>),
    While(Box<Expr>, Vec<Statement>),
    If(Box<Expr>, Vec<Statement>),
    For(BString, Box<Expr>, Box<Expr>, Vec<Statement>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    LitI64(i64),
    Variable(BString),
    BinaryOperation {
        lhs: Box<Expr>,
        operator: Operator,
        rhs: Box<Expr>,
    },
    Call {
        fnname: BString,
        args: Vec<Expr>,
    },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}
