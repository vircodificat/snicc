use bstr::BString;

use crate::asm;
use crate::tac;

use crate::asm::Reg;

pub fn assemble(prog: &tac::Program) -> asm::Program {
    let mut assmebler = Assembler::new();

    assmebler.assemble(prog)
}

struct Assembler {
    idgen: u64,
    instrs: Vec<asm::Instr>,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            idgen: 0,
            instrs: Vec::new(),
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
        self.instrs = Vec::new();
        for instr in &func.instrs {
            match instr {
                tac::Instr::Ret(ssa) => {
                    self.instrs.push(asm::Instr::Mv(Reg::ra(), todo!()));
                    self.instrs.push(asm::Instr::Ret);
                }
                tac::Instr::Call(ssa, l) => {
                    self.instrs.push(asm::Instr::Call(l.clone()));
                    todo!()
                }
                tac::Instr::Const(dst, v) => {
                    self.instrs
                        .push(asm::Instr::Li(Reg::t0(), u32::try_from(*v).unwrap()));
                }
                _ => todo!(),
            }
        }

        let mut instrs = Vec::new();
        std::mem::swap(&mut instrs, &mut self.instrs);

        asm::Func {
            visibility: asm::Visibility::Public,
            name: func.name.clone(),
            instrs,
        }
    }

    fn load_ssa(&mut self, ssa: tac::Ssa) {}
}
