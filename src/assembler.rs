use std::collections::HashMap;

use bstr::BString;

use crate::asm;
use crate::ast::Operator;
use crate::tac;

use crate::asm::Reg;

pub fn assemble(prog: &tac::Program) -> asm::Program {
    let mut assmebler = Assembler::new();

    assmebler.assemble(prog)
}

struct Assembler {
    idgen: u64,
    ssa_stack_offsets: HashMap<tac::Ssa, usize>,
    current_func_stack_space: isize,
    current_func_name: BString,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            idgen: 0,
            ssa_stack_offsets: HashMap::new(),
            current_func_stack_space: 0,
            current_func_name: BString::new(vec![]),
        }
    }

    fn assemble(&mut self, prog: &tac::Program) -> asm::Program {
        let mut funcs = vec![];

        for func in &prog.funcs {
            funcs.push(self.assemble_func(func));
        }

        asm::Program { funcs }
    }

    fn gensym(&mut self) -> BString {
        let id = BString::from(format!("gen_{}", self.idgen));
        self.idgen += 1;
        id
    }

    fn assemble_func(&mut self, func: &tac::Func) -> asm::Func {
        self.ssa_stack_offsets.clear();
        self.current_func_name = func.name.clone();
        let mut instrs = Vec::new();
        self.current_func_stack_space = self.assemble_func_alloca(func, &mut instrs);
        for block in &func.blocks {
            self.assemble_block(block, &mut instrs);
        }

        asm::Func {
            visibility: asm::Visibility::Public,
            name: func.name.clone(),
            instrs,
        }
    }

    fn assemble_func_alloca(&mut self, func: &tac::Func, instrs: &mut Vec<asm::Instr>) -> isize {
        let mut total_size = 0isize;
        for block in &func.blocks {
            for instr in &block.instrs {
                if let Some(ssa) = instr.dst() {
                    self.ssa_stack_offsets.insert(ssa, total_size as usize);
                    eprintln!("{ssa} => {total_size}(sp) FROM {instr}");
                    let size = 8isize;
                    total_size += size as isize;
                }
            }
        }
        instrs.push(asm::Instr::OpImm(
            asm::Op::Add,
            Reg::sp(),
            Reg::sp(),
            (-total_size) as i32,
        ));
        total_size
    }

    fn assemble_block(&mut self, block: &tac::Block, instrs: &mut Vec<asm::Instr>) {
        instrs.push(asm::Instr::Label(block.name.clone()));
        for instr in &block.instrs {
            match instr {
                tac::Instr::Ret(ssa) => {
                    let stack_offset = self.ssa_stack_offsets[ssa];
                    let total_size: isize = self.current_func_stack_space;
                    instrs.push(asm::Instr::Load(Reg::a0(), Reg::sp(), stack_offset as u32));
                    instrs.push(asm::Instr::OpImm(
                        asm::Op::Add,
                        Reg::sp(),
                        Reg::sp(),
                        total_size as i32,
                    ));
                    instrs.push(asm::Instr::Ret);
                }
                tac::Instr::Call(dst, l, args) => {
                    for (i, arg) in args.iter().enumerate() {
                        let stack_offset = self.ssa_stack_offsets[arg];
                        instrs.push(asm::Instr::Load(
                            Reg::X(10 + i as u32),
                            Reg::sp(),
                            stack_offset as u32,
                        ));
                    }
                    instrs.push(asm::Instr::Call(l.clone()));
                    let dst_stack_offset = self.ssa_stack_offsets[dst];
                    instrs.push(asm::Instr::Store(
                        Reg::a0(),
                        Reg::sp(),
                        dst_stack_offset as u32,
                    ));
                }
                tac::Instr::Const(dst, v) => {
                    let stack_offset = self.ssa_stack_offsets[dst];
                    instrs.push(asm::Instr::Li(Reg::t0(), u32::try_from(*v).unwrap()));
                    instrs.push(asm::Instr::Store(Reg::t0(), Reg::sp(), stack_offset as u32));
                }
                tac::Instr::Alloca(dst, size, _name) => (),
                tac::Instr::Print(ssa) => {
                    let stack_offset = self.ssa_stack_offsets[ssa];
                    instrs.push(asm::Instr::Load(Reg::a0(), Reg::sp(), stack_offset as u32));
                    instrs.push(asm::Instr::Call(BString::from("snicc_print")));
                }
                tac::Instr::Exit => {
                    instrs.push(asm::Instr::Li(Reg::a0(), 0));
                    instrs.push(asm::Instr::Call(BString::from("snicc_exit")));
                    instrs.push(asm::Instr::EBreak);
                }
                tac::Instr::Store(val_ssa, addr_ssa) => {
                    let val_stack_offset = self.ssa_stack_offsets[val_ssa];
                    let addr_stack_offset = self.ssa_stack_offsets[addr_ssa];

                    instrs.push(asm::Instr::Load(
                        Reg::t0(),
                        Reg::sp(),
                        val_stack_offset as u32,
                    ));
                    instrs.push(asm::Instr::Store(
                        Reg::t0(),
                        Reg::sp(),
                        addr_stack_offset as u32,
                    ));
                }
                tac::Instr::Load(dst_ssa, addr_ssa) => {
                    let addr_stack_offset = self.ssa_stack_offsets[addr_ssa];
                    let dst_stack_offset = self.ssa_stack_offsets[dst_ssa];
                    instrs.push(asm::Instr::Load(
                        Reg::t0(),
                        Reg::sp(),
                        addr_stack_offset as u32,
                    ));
                    instrs.push(asm::Instr::Store(
                        Reg::t0(),
                        Reg::sp(),
                        dst_stack_offset as u32,
                    ));
                }
                tac::Instr::BinOp(dst_ssa, operator, lhs_ssa, rhs_ssa) => {
                    let dst_stack_offset = self.ssa_stack_offsets[dst_ssa];
                    let lhs_stack_offset = self.ssa_stack_offsets[lhs_ssa];
                    let rhs_stack_offset = self.ssa_stack_offsets[rhs_ssa];
                    instrs.push(asm::Instr::Load(
                        Reg::t1(),
                        Reg::sp(),
                        lhs_stack_offset as u32,
                    ));
                    instrs.push(asm::Instr::Load(
                        Reg::t2(),
                        Reg::sp(),
                        rhs_stack_offset as u32,
                    ));
                    match operator {
                        Operator::Add => instrs.push(asm::Instr::Op(
                            asm::Op::Add,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        Operator::Sub => instrs.push(asm::Instr::Op(
                            asm::Op::Sub,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        Operator::Mul => instrs.push(asm::Instr::Op(
                            asm::Op::Mul,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        Operator::Div => instrs.push(asm::Instr::Op(
                            asm::Op::Div,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        Operator::Eq => {
                            instrs.push(asm::Instr::Op(
                                asm::Op::Sub,
                                Reg::t0(),
                                Reg::t1(),
                                Reg::t2(),
                            ));
                            instrs.push(asm::Instr::Li(Reg::t1(), 1));
                            instrs.push(asm::Instr::Op(asm::Op::Sltu, Reg::t0(), Reg::t0(), Reg::t1()));
                        }
                        Operator::NotEq => {
                            instrs.push(asm::Instr::Op(
                                asm::Op::Sub,
                                Reg::t0(),
                                Reg::t1(),
                                Reg::t2(),
                            ));
                            instrs.push(asm::Instr::Li(Reg::t1(), 1));
                            instrs.push(asm::Instr::Op(
                                asm::Op::Sltu,
                                Reg::t0(),
                                Reg::t0(),
                                Reg::t1(),
                            ));
                        }
                        Operator::Lt => {
                            instrs.push(asm::Instr::Op(
                                asm::Op::Slt,
                                Reg::t0(),
                                Reg::t1(),
                                Reg::t2(),
                            ));
                        }
                        /*
                        Operator::Lt => instrs.push(asm::Instr::Op(
                            asm::Op::Lt,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        Operator::Gt => instrs.push(asm::Instr::Op(
                            asm::Op::Gt,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        Operator::LtEq => instrs.push(asm::Instr::Op(
                            asm::Op::LtEq,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        Operator::GtEq => instrs.push(asm::Instr::Op(
                            asm::Op::GtEq,
                            Reg::t0(),
                            Reg::t1(),
                            Reg::t2(),
                        )),
                        */
                        _ => {
                            dbg!(operator);
                            todo!()
                        }
                    };
                    instrs.push(asm::Instr::Store(
                        Reg::t0(),
                        Reg::sp(),
                        dst_stack_offset as u32,
                    ));
                }
                tac::Instr::Beqz(cond_ssa, zero_block_id, nonzero_block_id) => {
                    let cond_stack_offset = self.ssa_stack_offsets[cond_ssa];
                    instrs.push(asm::Instr::Load(
                        Reg::t0(),
                        Reg::sp(),
                        cond_stack_offset as u32,
                    ));
                    instrs.push(asm::Instr::Beqz(
                        Reg::t0(),
                        format!("{}.{}", &self.current_func_name, zero_block_id.0).into(),
                    ));
                    instrs.push(asm::Instr::Jump(
                        format!("{}.{}", &self.current_func_name, nonzero_block_id.0).into(),
                    ));
                }
                tac::Instr::Goto(block_id, ssas) => {
                    assert!(ssas.is_empty());
                    instrs.push(asm::Instr::Jump(
                        format!("{}.{}", &self.current_func_name, block_id.0).into(),
                    ));
                }
            }
        }
    }

    fn load_ssa(&mut self, ssa: tac::Ssa) {}
}
