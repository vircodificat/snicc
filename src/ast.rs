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
    pub stmts: Vec<Statement>,
}


#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Variable {
        name: BString,
    },
    VariableInit {
        name: BString,
        expr: Box<Expr>,
    },
    Print {
        expr: Box<Expr>,
    },
    Exit,
    Ret(Box<Expr>),
    Expr(Box<Expr>),
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
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}
