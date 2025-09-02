use std::collections::HashMap;

use crate::{ast::Operator, tac};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FnIdx(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct InstrIdx(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Pc(FnIdx, InstrIdx);

#[derive(Debug)]
pub struct TacVm<'a> {
    pc: Pc,
    program: &'a tac::Program,
    stack: Vec<StackFrame>,
    halted: bool,
}

#[derive(Debug)]
pub struct StackFrame {
    return_pc: Pc,
    return_ssa: tac::Ssa,
    ssa_value: HashMap<tac::Ssa, Value>,
    locals: Vec<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    I64(i64),
    Addr(usize),
    Undefined,
}

impl<'a> TacVm<'a> {
    pub fn new(program: &'a tac::Program) -> TacVm<'a> {
        let main_frame = StackFrame {
            return_pc: Pc::no_return(),
            return_ssa: tac::Ssa(0),
            ssa_value: HashMap::new(),
            locals: vec![],
        };

        TacVm {
            pc: Pc(FnIdx(0), InstrIdx(0)),
            program,
            stack: vec![main_frame],
            halted: false,
        }
    }

    pub fn step(&mut self) {
        let func = &self.program.funcs[self.pc.0.0];
        let instr = &func.instrs[self.pc.1.0];
        //eprintln!("STEP {instr}");

        match instr {
            tac::Instr::Alloca(ssa, size, bstring) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let addr = Value::Addr(frame.locals.len());
                frame.locals.push(Value::Undefined);
                frame.ssa_value.insert(*ssa, addr);
                self.advance_pc();
            }
            tac::Instr::Call(ssa, fnname) => {
                let func_idx = 'result: {
                    for (i, func) in self.program.funcs.iter().enumerate() {
                        if func.name == *fnname {
                            break 'result FnIdx(i);
                        }
                    }
                    panic!("No such function")
                };

                let frame = StackFrame {
                    return_pc: self.pc.next(),
                    return_ssa: *ssa,
                    ssa_value: HashMap::new(),
                    locals: vec![],
                };
                self.stack.push(frame);
                self.pc = Pc(func_idx, InstrIdx(0));
            }
            tac::Instr::Ret(ssa) => {
                let frame = &self.stack.last_mut().unwrap();
                self.pc = frame.return_pc;
                let return_ssa = frame.return_ssa;
                let return_val = *frame.ssa_value.get(ssa).unwrap();
                self.stack.pop();

                if let Some(frame) = &mut self.stack.last_mut() {
                    frame.ssa_value.insert(return_ssa, return_val);
                } else {
                    self.halted = true;
                }
            }
            tac::Instr::Const(ssa, val) => {
                let frame = &mut self.stack.last_mut().unwrap();
                frame.ssa_value.insert(*ssa, Value::I64(*val));
                self.advance_pc();
            }
            tac::Instr::Store(src, addr) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let Value::Addr(addr) = *frame.ssa_value.get(addr).unwrap() else {
                    panic!("addr = {addr}");
                };
                let val = *frame.ssa_value.get(src).unwrap();
                frame.locals[addr] = val;

                self.advance_pc();
            }
            tac::Instr::Load(dst, addr) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let Value::Addr(addr) = *frame.ssa_value.get(addr).unwrap() else {
                    panic!();
                };
                let val = frame.locals[addr];
                frame.ssa_value.insert(*dst, val);

                self.advance_pc();
            }
            tac::Instr::Print(ssa) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let val = frame.ssa_value.get(ssa).unwrap();
                println!("{val:?}");
                self.advance_pc();
            }
            tac::Instr::BinOp(dst, op, lhs_ssa, rhs_ssa) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let Value::I64(lhs_val) = frame.ssa_value.get(lhs_ssa).unwrap().clone() else { panic!() };
                let Value::I64(rhs_val) = frame.ssa_value.get(rhs_ssa).unwrap().clone() else { panic!() };
                let result_val = Value::I64(match op {
                    Operator::Add => lhs_val + rhs_val,
                    Operator::Sub => lhs_val - rhs_val,
                    Operator::Mul => lhs_val * rhs_val,
                    Operator::Div => lhs_val / rhs_val,
                });
                frame.ssa_value.insert(*dst, result_val);
                self.advance_pc();
            }
        }
    }

    fn advance_pc(&mut self) {
        self.pc = self.pc.next();
    }

    pub fn run(&mut self) {
        while !self.halted {
            self.step();
        }
    }
}

impl Pc {
    pub fn func(&self) -> FnIdx {
        self.0
    }
    pub fn instr(&self) -> InstrIdx {
        self.1
    }
    pub fn no_return() -> Pc {
        Pc(FnIdx(usize::MAX), InstrIdx(usize::MAX))
    }
    pub fn next(&self) -> Pc {
        Pc(self.func(), InstrIdx(self.instr().0 + 1))
    }
}
