use std::io::Write;

use bstr::{BStr, BString};

#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct Func {
    pub visibility: Visibility,
    pub name: BString,
    pub instrs: Vec<Instr>,
}

pub enum Instr {
    Label(BString),
    Call(BString),
    Ret,
    EBreak,
    Load(Reg, Reg, u32),
    Store(Reg, Reg, u32),
    Op(Op, Reg, Reg, Reg),
    OpImm(Op, Reg, Reg, i32),
    Mv(Reg, Reg),
    Li(Reg, u32),
    Beqz(Reg, BString),
    Jump(BString),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Slt,
    Sltu,
}

#[derive(Debug, Clone)]
pub enum Reg {
    X(u32),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Visibility {
    Private,
    Public,
}

#[rustfmt::skip]
impl Reg {
    pub fn zero() -> Reg { Reg::X(0) }
    pub fn ra()   -> Reg { Reg::X(1) }
    pub fn sp()   -> Reg { Reg::X(2) }

    pub fn a0()   -> Reg { Reg::X(10) }
    pub fn a1()   -> Reg { Reg::X(11) }
    pub fn a2()   -> Reg { Reg::X(12) }
    pub fn a3()   -> Reg { Reg::X(13) }
    pub fn a4()   -> Reg { Reg::X(14) }
    pub fn a5()   -> Reg { Reg::X(15) }
    pub fn a6()   -> Reg { Reg::X(16) }
    pub fn a7()   -> Reg { Reg::X(17) }

    pub fn t0()   -> Reg { Reg::X(5) }
    pub fn t1()   -> Reg { Reg::X(6) }
    pub fn t2()   -> Reg { Reg::X(7) }
    pub fn t3()   -> Reg { Reg::X(28) }
    pub fn t4()   -> Reg { Reg::X(29) }
    pub fn t5()   -> Reg { Reg::X(30) }
    pub fn t6()   -> Reg { Reg::X(31) }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::X(0) => write!(f, "zero"),
            Reg::X(1) => write!(f, "ra"),
            Reg::X(2) => write!(f, "sp"),
            Reg::X(10) => write!(f, "a0"),
            Reg::X(11) => write!(f, "a1"),
            Reg::X(12) => write!(f, "a2"),
            Reg::X(13) => write!(f, "a3"),
            Reg::X(14) => write!(f, "a4"),
            Reg::X(15) => write!(f, "a5"),
            Reg::X(16) => write!(f, "a6"),
            Reg::X(17) => write!(f, "a7"),
            Reg::X(5) => write!(f, "t0"),
            Reg::X(6) => write!(f, "t1"),
            Reg::X(7) => write!(f, "t2"),
            Reg::X(28) => write!(f, "t3"),
            Reg::X(29) => write!(f, "t4"),
            Reg::X(30) => write!(f, "t5"),
            Reg::X(31) => write!(f, "t6"),
            Reg::X(i) => write!(f, "x{i}"),
        }
    }
}

pub fn write_asm<W>(mut f: &mut W, prog: &Program) -> std::io::Result<()>
where
    W: std::io::Write,
{
    for func in &prog.funcs {
        write_asm_func(&mut f, func)?;
    }

    Ok(())
}

pub fn write_asm_func<W>(mut f: W, func: &Func) -> std::io::Result<()>
where
    W: std::io::Write,
{
    let name = &func.name;
    if func.visibility == Visibility::Public {
        writeln!(f, ".globl {name}")?;
    }
    writeln!(f, "{name}:")?;
    for instr in &func.instrs {
        write_asm_instr(&mut f, &instr)?;
    }
    Ok(())
}

pub fn write_asm_instr<W>(mut f: W, instr: &Instr) -> std::io::Result<()>
where
    W: std::io::Write,
{
    match instr {
        Instr::Label(name) => {
            writeln!(f, "{name}:")?;
        }
        Instr::Ret => {
            writeln!(f, "    ret")?;
        }
        Instr::Call(label) => {
            writeln!(f, "    call {label}")?;
        }
        Instr::Op(op, rd, rs1, rs2) => writeln!(f, "    {op} {rd}, {rs1}, {rs2}")?,
        Instr::OpImm(op, rd, rs1, imm) => writeln!(f, "    {op}i {rd}, {rs1}, {imm}")?,
        Instr::Load(rd, rs1, imm) => {}
        Instr::Store(rs1, rs2, imm) => {}
        Instr::Mv(_, _) => todo!(),
        Instr::Li(_, _) => todo!(),
        Instr::Beqz(rs1, lbl) => writeln!(f, "beqz {rs1}, {lbl}")?,
        Instr::EBreak => writeln!(f, "ebreak")?,
        Instr::Jump(lbl) => writeln!(f, "j {lbl}")?,
    }
    Ok(())
}

impl Program {
    pub fn resolve(self) -> Self {
        let mut funcs = vec![];
        for func in self.funcs {
            funcs.push(func.resolve());
        }
        Program { funcs }
    }

    pub(crate) fn func(&self, name: &BStr) -> &Func {
        for func in &self.funcs {
            if func.name == name {
                return &func;
            }
        }
        panic!("No such func: {name}")
    }
}

impl Func {
    pub fn resolve(self) -> Self {
        self
    }
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Label(lbl) => write!(f, "{lbl}:"),
            Instr::Call(l) => write!(f, "call {l}"),
            Instr::Ret => write!(f, "ret"),
            Instr::Op(op, rd, rs1, rs2) => write!(f, "{op} {rd}, {rs1}, {rs2}"),
            Instr::OpImm(op, rd, rs1, imm) => write!(f, "{op}i {rd}, {rs1}, {imm}"),
            Instr::Load(rd, rs1, imm) => write!(f, "ld {rd}, {imm}({rs1})"),
            Instr::Store(rs2, rs1, imm) => write!(f, "sd {rs2}, {imm}({rs1})"),
            Instr::Mv(rd, imm) => write!(f, "mv {rd}, {imm}"),
            Instr::Li(rd, imm) => write!(f, "li {rd}, {imm}"),
            Instr::Beqz(rs1, lbl) => write!(f, "beqz {rs1}, {lbl}"),
            Instr::EBreak => write!(f, "ebreak"),
            Instr::Jump(lbl) => write!(f, "j {lbl}"),
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "add"),
            Op::Sub => write!(f, "sub"),
            Op::Mul => write!(f, "mul"),
            Op::Div => write!(f, "div"),
            Op::Slt => write!(f, "slt"),
            Op::Sltu => write!(f, "sltu"),
        }
    }
}

macro_rules! debug_from_display {
    ($typ:path) => {
        impl std::fmt::Debug for $typ {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "\"{self}\"")
            }
        }
    };
}

debug_from_display!(Instr);

impl Program {
    pub fn pprint<W: Write>(&self, f: &mut W) -> std::io::Result<()> {
        for func in &self.funcs {
            func.pprint(f)?;
        }
        Ok(())
    }
}

impl Func {
    pub fn pprint<W: Write>(&self, f: &mut W) -> std::io::Result<()> {
        if self.visibility == Visibility::Public {
            writeln!(f, ".globl {}", self.name)?;
        }
        writeln!(f, "{}:", self.name)?;
        for instr in &self.instrs {
            if let Instr::Label(_lbl) = instr {
                writeln!(f, "{instr}")?;
            } else {
                writeln!(f, "    {instr}")?;
            }
        }
        Ok(())
    }
}
