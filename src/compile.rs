use std::collections::HashMap;

use bstr::BString;

use crate::ast;
use crate::tac;

use crate::tac::BlockId;
use crate::tac::Instr;
use crate::tac::Ssa;

pub fn compile(prog: &ast::Program) -> tac::Program {
    let mut compiler = Compiler::new();

    compiler.compile(prog)
}

struct Compiler {
    idgen: u32,
    variable_ssas: HashMap<BString, Ssa>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            idgen: 0,
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

        self.variable_ssas = HashMap::new();

        let mut params: Vec<tac::Param> = vec![];
        for name in func.params.clone() {
            let ssa = self.gensym();
            let param = tac::Param { ssa, name };
            params.push(param);
        }

        let mut first_block = tac::Block {
            name: "BLOCK".into(),
            instrs: vec![],
            params: params.clone(),
        };

        let mut blocks = vec![first_block];

        let first_block = blocks.last_mut().unwrap();
        for param in &params {
            let addr_ssa = self.gensym();
            first_block.instrs.push(Instr::Alloca(
                addr_ssa,
                8,
                Some(param.name.clone()),
            ));
            first_block.instrs.push(Instr::Store(
                param.ssa,
                addr_ssa,
            ));
            self.variable_ssas.insert(param.name.clone(), addr_ssa);
        }

        for stmt in &func.stmts {
            self.compile_statement(stmt, &mut blocks);
        }

        tac::Func {
            visibility: tac::Visibility::Public,
            name: func.name.clone(),
            blocks,
        }
    }

    fn compile_statement(&mut self, stmt: &ast::Statement, blocks: &mut Vec<tac::Block>) {
        match stmt {
            ast::Statement::Variable { name } => {
                let var_ssa = self.gensym();
                let current_block = blocks.last_mut().unwrap();
                current_block
                    .instrs
                    .push(Instr::Alloca(var_ssa, 8, Some(name.clone())));
                self.variable_ssas.insert(name.clone(), var_ssa);
            }
            ast::Statement::VariableInit { name, expr } => {
                let current_block = blocks.last_mut().unwrap();
                let var_ssa = self.gensym();
                let val_ssa = self.compile_expr(expr, current_block);
                let current_block = blocks.last_mut().unwrap();
                current_block
                    .instrs
                    .push(Instr::Alloca(var_ssa, 8, Some(name.clone())));
                current_block.instrs.push(Instr::Store(val_ssa, var_ssa));
                self.variable_ssas.insert(name.clone(), var_ssa);
            }
            ast::Statement::Assign { name, expr } => {
                let current_block = blocks.last_mut().unwrap();
                let val_ssa = self.compile_expr(expr, current_block);
                let var_ssa = self.variable_ssas.get(name).unwrap();
                let current_block = blocks.last_mut().unwrap();
                current_block.instrs.push(Instr::Store(val_ssa, *var_ssa));
            }
            ast::Statement::Print { expr } => {
                let current_block = blocks.last_mut().unwrap();
                let val_ssa = self.compile_expr(expr, current_block);
                current_block.instrs.push(Instr::Print(val_ssa));
            }
            ast::Statement::Ret(e) => {
                let current_block = blocks.last_mut().unwrap();
                let val = self.compile_expr(e, current_block);
                current_block.instrs.push(Instr::Ret(val));
            }
            ast::Statement::Expr(expr) => {
                let current_block = blocks.last_mut().unwrap();
                self.compile_expr(expr, current_block);
            }
            ast::Statement::Exit => {
                let current_block = blocks.last_mut().unwrap();
                current_block.instrs.push(Instr::Exit);
            }
            ast::Statement::Loop(stmts) => {
                let loop_start_block_id = BlockId(blocks.len() as u32);
                let current_block = blocks.last_mut().unwrap();

                current_block
                    .instrs
                    .push(Instr::Goto(loop_start_block_id, vec![]));
                blocks.push(tac::Block {
                    name: "BLOCK".into(),
                    instrs: vec![],
                    params: vec![],
                });
                for stmt in stmts {
                    self.compile_statement(stmt, blocks);
                }
                let loop_block = blocks.last_mut().unwrap();
                loop_block
                    .instrs
                    .push(Instr::Goto(loop_start_block_id, vec![]));
                blocks.push(tac::Block {
                    name: "BLOCK".into(),
                    instrs: vec![],
                    params: vec![],
                });
            }
            ast::Statement::If(expr, stmts) => {
                let if_start_block_id = BlockId(blocks.len() as u32);
                let if_end_block_id = BlockId((blocks.len() + 1) as u32);

                let current_block = blocks.last_mut().unwrap();
                let condition_ssa = self.compile_expr(expr, current_block);

                current_block.instrs.push(Instr::Beqz(
                    condition_ssa,
                    if_end_block_id,
                    if_start_block_id,
                ));

                blocks.push(tac::Block {
                    name: "BLOCK".into(),
                    instrs: vec![],
                    params: vec![],
                });

                for stmt in stmts {
                    self.compile_statement(stmt, blocks);
                }

                let current_block = blocks.last_mut().unwrap();
                current_block
                    .instrs
                    .push(Instr::Goto(if_end_block_id, vec![]));

                blocks.push(tac::Block {
                    name: "BLOCK".into(),
                    instrs: vec![],
                    params: vec![],
                });
            }
        }
    }

    fn compile_expr(&mut self, e: &ast::Expr, block: &mut tac::Block) -> Ssa {
        match e {
            ast::Expr::LitI64(v) => {
                let ssa = self.gensym();
                block.instrs.push(Instr::Const(ssa, *v));
                ssa
            }
            ast::Expr::Variable(name) => {
                let ssa = self.gensym();
                let variable_ssa = *self.variable_ssas.get(name).unwrap();
                block.instrs.push(Instr::Load(ssa, variable_ssa));
                ssa
            }
            ast::Expr::BinaryOperation { lhs, operator, rhs } => {
                let ssa = self.gensym();
                let lhs_ssa = self.compile_expr(lhs, block);
                let rhs_ssa = self.compile_expr(rhs, block);
                block
                    .instrs
                    .push(Instr::BinOp(ssa, *operator, lhs_ssa, rhs_ssa));
                ssa
            }
            ast::Expr::Call { fnname, args } => {
                let ssa = self.gensym();
                let mut arg_ssas = Vec::new();
                for arg in args {
                    arg_ssas.push(self.compile_expr(arg, block));
                }
                block
                    .instrs
                    .push(Instr::Call(ssa, fnname.clone(), arg_ssas));
                ssa
            }
        }
    }
}
