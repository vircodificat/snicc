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
    Call(BString),
    Ret,
    Load(Reg, Reg, u32),
    Store(Reg, Reg, u32),
    Op(Op, Reg, Reg, Reg),
    OpI(Op, Reg, Reg, u32),
    Mv(Reg, Reg),
    Li(Reg, u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
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
        Instr::Ret => {
            writeln!(f, "    ret")?;
        }
        Instr::Call(label) => {
            writeln!(f, "    call {label}")?;
        }
        Instr::Op(op, rd, rs1, rs2) => writeln!(f, "    {op} {rd}, {rs1}, {rs2}")?,
        Instr::OpI(op, rd, rs1, imm) => writeln!(f, "    {op}i {rd}, {rs1}, {imm}")?,
        Instr::Load(rd, rs1, imm) => {}
        Instr::Store(rs1, rs2, imm) => {}
        Instr::Mv(_, _) => todo!(),
        Instr::Li(_, _) => todo!(),
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
            Instr::Call(l) => write!(f, "call {l}"),
            Instr::Ret => write!(f, "ret"),
            Instr::Op(op, rd, rs1, rs2) => write!(f, "{op} {rd}, {rs1}, {rs2}"),
            Instr::OpI(op, rd, rs1, imm) => write!(f, "{op}i {rd}, {rs1}, {imm}"),
            Instr::Load(_, _, _) => todo!(),
            Instr::Store(_, _, _) => todo!(),
            Instr::Mv(_, _) => todo!(),
            Instr::Li(_, _) => todo!(),
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "add"),
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
