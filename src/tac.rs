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
    Alloca(Ssa, u8, Option<BString>),
    Call(Ssa, BString),
    Ret(Ssa),
    Const(Ssa, i64),
    Load(Ssa, Ssa),
    Store(Ssa, Ssa),
    Print(Ssa),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Ssa(pub u32); 

#[derive(Debug)]
pub enum Op {
    Negate,
    Invert,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Visibility {
    Private,
    Public,
}

impl std::fmt::Display for Ssa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Program {
    pub fn resolve(self) -> Self {
        let mut funcs = vec![];
        for func in self.funcs {
            funcs.push(func.resolve());
        }
        Program {
            funcs,
        }
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
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Call(ssa, l) =>            write!(f, "CALL   {ssa}, {l}"),
            Instr::Ret(v) =>                  write!(f, "RET    {v}"),
            Instr::Const(dst, v) =>           write!(f, "CONST  {dst}, {v}"),
            Instr::Alloca(ssa, size, name) => {
                                                      write!(f, "ALLOCA {ssa}, {size}", )?;
                        if let Some(name_hint) = name {
                            write!(f, " ({name_hint})")?;
                        }
                        Ok(())
                    }
            Instr::Load(dst, addr) =>         write!(f, "LOAD   {dst}, ({addr})"),
            Instr::Store(src, addr) =>        write!(f, "STORE  {src}, ({addr})"),
            Instr::Print(ssa) =>              write!(f, "PRINT  {ssa}"),
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Negate => write!(f, "Negate"),
            Op::Invert => write!(f, "Invert"),
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
            write!(f, "pub ")?;
        }
        writeln!(f, "fn {}", &self.name)?;

        for instr in &self.instrs {
            writeln!(f, "    {instr}");
        }
        Ok(())
    }
}
