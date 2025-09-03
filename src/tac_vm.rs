const DEBUG: bool = false;

use std::collections::HashMap;

use crate::{ast::Operator, tac};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FnIdx(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BlockIdx(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct InstrIdx(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Pc(FnIdx, BlockIdx, InstrIdx);


#[derive(Debug)]
pub struct TacVm<'a> {
    pc: Pc,
    program: &'a tac::Program,
    stack: Vec<StackFrame>,
    halted: bool,
    trapped: bool,
    debug: bool,
}

#[derive(Debug)]
pub struct StackFrame {
    return_pc: Pc,
    return_ssa: tac::Ssa,
    ssa_values: HashMap<tac::Ssa, Value>,
    locals: Vec<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    I64(i64),
    Addr(usize),
    Uninitialized,
}

impl<'a> TacVm<'a> {
    pub fn new(program: &'a tac::Program) -> TacVm<'a> {
        let main_frame = StackFrame {
            return_pc: Pc::no_return(),
            return_ssa: tac::Ssa(0),
            ssa_values: HashMap::new(),
            locals: vec![],
        };

        TacVm {
            pc: Pc(FnIdx(0), BlockIdx(0), InstrIdx(0)),
            program,
            stack: vec![main_frame],
            halted: false,
            trapped: false,
            debug: DEBUG,
        }
    }

    fn ssa_value(&self, ssa: tac::Ssa) -> Value {
        let frame = &mut self.stack.last().unwrap();
        *frame.ssa_values.get(&ssa).unwrap()
    }

    fn ssa_set_value(&mut self, ssa: tac::Ssa, value: Value) {
        let frame = &mut self.stack.last_mut().unwrap();
        let old_value = frame.ssa_values.insert(ssa, value);
        if old_value.is_some() {
            self.trap();
        }
    }

    fn alloca(&mut self, size: u8) -> Value {
        let frame = &mut self.stack.last_mut().unwrap();
        let addr = Value::Addr(frame.locals.len());
        frame.locals.push(Value::Uninitialized);
        addr
    }

    pub fn step(&mut self) {
        let func = &self.program.funcs[self.pc.0.0];
        let block = &func.blocks[self.pc.1.0];
        let instr = &block.instrs[self.pc.2.0];
        //eprintln!("STEP {instr}");

        match instr {
            tac::Instr::Alloca(ssa, size, bstring) => {
                let value = self.alloca(*size);
                self.ssa_set_value(*ssa, value);
                self.advance_pc();
            }
            tac::Instr::Call(ssa, fnname, arg_ssas) => {
                let func_idx = 'result: {
                    for (i, func) in self.program.funcs.iter().enumerate() {
                        if func.name == *fnname {
                            break 'result FnIdx(i);
                        }
                    }
                    return self.trap();
                };
                let callee_func = &self.program.funcs[func_idx.0];
                let mut ssa_values = HashMap::new();
                for (i, (param, arg_ssa)) in callee_func
                    .params()
                    .into_iter()
                    .zip(arg_ssas.iter())
                    .enumerate()
                {
                    let val = self.ssa_value(*arg_ssa);
                    ssa_values.insert(param.ssa, val);
                }

                let frame = StackFrame {
                    return_pc: self.pc.next(),
                    return_ssa: *ssa,
                    ssa_values,
                    locals: vec![],
                };
                self.stack.push(frame);
                self.pc = Pc(func_idx, BlockIdx(0), InstrIdx(0));
            }
            tac::Instr::Exit => {
                self.stack.pop();
                self.halted = true;
            }
            tac::Instr::Ret(ssa) => {
                let frame = &self.stack.last_mut().unwrap();
                self.pc = frame.return_pc;
                let return_ssa = frame.return_ssa;
                let return_val = *frame.ssa_values.get(ssa).unwrap();
                self.pc = frame.return_pc;
                self.stack.pop();

                if let Some(frame) = &mut self.stack.last_mut() {
                    frame.ssa_values.insert(return_ssa, return_val);
                } else {
                    self.halted = true;
                }
            }
            tac::Instr::Const(ssa, val) => {
                let frame = &mut self.stack.last_mut().unwrap();
                frame.ssa_values.insert(*ssa, Value::I64(*val));
                self.advance_pc();
            }
            tac::Instr::Store(src, addr) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let Value::Addr(addr) = *frame.ssa_values.get(addr).unwrap() else {
                    return self.trap();
                };
                let val = *frame.ssa_values.get(src).unwrap();
                frame.locals[addr] = val;

                self.advance_pc();
            }
            tac::Instr::Load(dst, addr_ssa) => {
                let addr = self.ssa_value(*addr_ssa);
                let Value::Addr(addr) = addr else {
                    return self.trap();
                };
                let frame = &mut self.stack.last_mut().unwrap();
                let val = frame.locals[addr];
                frame.ssa_values.insert(*dst, val);

                self.advance_pc();
            }
            tac::Instr::Print(ssa) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let val = frame.ssa_values.get(ssa).unwrap();
                println!("{val:?}");
                self.advance_pc();
            }
            tac::Instr::BinOp(dst, op, lhs_ssa, rhs_ssa) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let Value::I64(lhs_val) = frame.ssa_values.get(lhs_ssa).unwrap().clone() else {
                    return self.trap();
                };
                let Value::I64(rhs_val) = frame.ssa_values.get(rhs_ssa).unwrap().clone() else {
                    return self.trap();
                };
                let result_val = Value::I64(match op {
                    Operator::Add => lhs_val + rhs_val,
                    Operator::Sub => lhs_val - rhs_val,
                    Operator::Mul => lhs_val * rhs_val,
                    Operator::Div => lhs_val / rhs_val,
                    Operator::Eq => (lhs_val == rhs_val) as i64,
                    Operator::Lt => (lhs_val < rhs_val) as i64,
                    Operator::Gt => (lhs_val > rhs_val) as i64,
                    Operator::LtEq => (lhs_val <= rhs_val) as i64,
                    Operator::GtEq => (lhs_val >= rhs_val) as i64,
                });
                frame.ssa_values.insert(*dst, result_val);
                self.advance_pc();
            }
            tac::Instr::Goto(block_id, ssas) => {
                self.pc = Pc(self.pc.func(), BlockIdx(block_id.0 as usize), InstrIdx(0));
            }
            tac::Instr::Beqz(ssa, bb_zero, bb_nonzero) => {
                let frame = &mut self.stack.last_mut().unwrap();
                let val = frame.ssa_values.get(ssa).unwrap();
                self.pc = if *val == Value::I64(0) {
                    self.pc.branch(BlockIdx(bb_zero.0 as usize))
                } else {
                    self.pc.branch(BlockIdx(bb_nonzero.0 as usize))
                };
            }
        }
    }

    fn advance_pc(&mut self) {
        self.pc = self.pc.next();
    }

    fn trap(&mut self) {
        eprintln!("TRAP at {:?}", self.pc);
        self.trapped = true;
        self.halted = true;
    }

    pub fn dump(&self) {
        println!("PC: {:?}", self.pc);
        let func = &self.program.funcs[self.pc.0.0];
        let blocks = &func.blocks[self.pc.1.0];
        let instr = &blocks.instrs[self.pc.2.0];
        println!("INSTR: {instr:?}");
        println!("Stack");
        for (i, frame) in self.stack.iter().enumerate() {
            println!("Frame #{i}");
            eprintln!("    Return PC:  {:?}", frame.return_pc);
            eprintln!("    Return SSA: {:?}", frame.return_ssa);
            if !frame.ssa_values.is_empty() {
                eprintln!("    SSAs:");
                for (ssa, value) in frame.ssa_values.iter() {
                    eprintln!("         {ssa} = {value:?}");
                }
            }
            if !frame.locals.is_empty() {
                eprintln!("    Locals:");
                for local in &frame.locals {
                    eprintln!("         {local:?}");
                }
            }
        }
        println!(
            "--------------------------------------------------------------------------------"
        );
        println!();
    }

    pub fn run(&mut self) {
        if self.debug {
            self.dump();
        }
        loop {
            self.step();

            if self.debug {
                self.dump();
            }

            if self.halted {
                break;
            }
        }
    }
}

impl Pc {
    pub fn func(&self) -> FnIdx {
        self.0
    }
    pub fn block(&self) -> BlockIdx {
        self.1
    }
    pub fn instr(&self) -> InstrIdx {
        self.2
    }
    pub fn no_return() -> Pc {
        Pc(
            FnIdx(usize::MAX),
            BlockIdx(usize::MAX),
            InstrIdx(usize::MAX),
        )
    }
    pub fn next(&self) -> Pc {
        Pc(self.func(), self.block(), InstrIdx(self.instr().0 + 1))
    }
    pub fn branch(&self, block_id: BlockIdx) -> Pc {
        Pc(self.func(), block_id, InstrIdx(0))
    }
}
