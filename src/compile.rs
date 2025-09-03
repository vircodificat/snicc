use std::collections::HashMap;

use bstr::BString;

use crate::ast;
use crate::tac;

use crate::tac::Instr;
use crate::tac::Ssa;

pub fn compile(prog: &ast::Program) -> tac::Program {
    let mut compiler = Compiler::new();

    compiler.compile(prog)
}

struct Compiler {
    idgen: u32,
    instrs: Vec<Instr>,
    variable_ssas: HashMap<BString, Ssa>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            idgen: 0,
            instrs: Vec::new(),
            variable_ssas: HashMap::new(),
        }
    }

    fn compile(&mut self, prog: &ast::Program) -> tac::Program {
        let mut funcs = vec![];

        for decl in &prog.decls {
            match decl {
                ast::Declaration::Fn(fndecl) => {
                    funcs.push(self.compile_func(fndecl));
                }
            }
        }

        tac::Program { funcs }
    }

    fn gensym(&mut self) -> Ssa {
        let id = Ssa(self.idgen);
        self.idgen += 1;
        id
    }

    fn compile_func(&mut self, func: &ast::FuncDecl) -> tac::Func {
        self.idgen = 0;
        self.instrs = Vec::new();
        self.variable_ssas = HashMap::new();
        for stmt in &func.stmts {
            match stmt {
                ast::Statement::Variable { name } => {
                    let var_ssa = self.gensym();
                    self.instrs
                        .push(Instr::Alloca(var_ssa, 8, Some(name.clone())));
                    self.variable_ssas.insert(name.clone(), var_ssa);
                }
                ast::Statement::VariableInit { name, expr } => {
                    let var_ssa = self.gensym();
                    let val_ssa = self.compile_expr(expr);
                    self.instrs
                        .push(Instr::Alloca(var_ssa, 8, Some(name.clone())));
                    self.instrs.push(Instr::Store(val_ssa, var_ssa));
                    self.variable_ssas.insert(name.clone(), var_ssa);
                }
                ast::Statement::Print { expr } => {
                    let val_ssa = self.compile_expr(expr);
                    self.instrs.push(Instr::Print(val_ssa));
                }
                ast::Statement::Ret(e) => {
                    let val = self.compile_expr(e);
                    self.instrs.push(Instr::Ret(val));
                }
                ast::Statement::Expr(expr) => {
                    self.compile_expr(expr);
                }
                ast::Statement::Exit => {
                    self.instrs.push(Instr::Exit);
                }
            }
        }

        let mut instrs = Vec::new();
        std::mem::swap(&mut instrs, &mut self.instrs);

        tac::Func {
            visibility: tac::Visibility::Public,
            name: func.name.clone(),
            instrs,
        }
    }

    fn compile_expr(&mut self, e: &ast::Expr) -> Ssa {
        match e {
            ast::Expr::LitI64(v) => {
                let ssa = self.gensym();
                self.instrs.push(Instr::Const(ssa, *v));
                ssa
            }
            ast::Expr::Variable(name) => {
                let ssa = self.gensym();
                let variable_ssa = *self.variable_ssas.get(name).unwrap();
                self.instrs.push(Instr::Load(ssa, variable_ssa));
                ssa
            }
            ast::Expr::BinaryOperation { lhs, operator, rhs } => {
                let ssa = self.gensym();
                let lhs_ssa = self.compile_expr(lhs);
                let rhs_ssa = self.compile_expr(rhs);
                self.instrs
                    .push(Instr::BinOp(ssa, *operator, lhs_ssa, rhs_ssa));
                ssa
            }
            ast::Expr::Call { fnname } => {
                let ssa = self.gensym();
                self.instrs.push(Instr::Call(ssa, fnname.clone()));
                ssa
            }
        }
    }
}
