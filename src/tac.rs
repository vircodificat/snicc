use bstr::{BStr, BString};
use std::io::Write;

use crate::ast::Operator;

#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct Func {
    pub visibility: Visibility,
    pub name: BString,
    pub blocks: Vec<Block>,
}

#[derive(Debug)]
pub struct Block {
    pub name: BString,
    pub params: Vec<Param>,
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Param {
    pub ssa: Ssa,
    pub name: BString,
}

pub enum Instr {
    Alloca(Ssa, u8, Option<BString>),
    Call(Ssa, BString, Vec<Ssa>),
    Exit,
    Ret(Ssa),
    Const(Ssa, i64),
    Load(Ssa, Ssa),
    Store(Ssa, Ssa),
    Print(Ssa),
    Beqz(Ssa, BlockId, BlockId),
    BinOp(Ssa, Operator, Ssa, Ssa),
    Goto(BlockId, Vec<Ssa>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Ssa(pub u32);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

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

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "^{}", self.0)
    }
}

impl Program {
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
    pub fn params(&self) -> Vec<Param> {
        self.blocks[0].params.clone()
    }
}

impl std::fmt::Display for Instr {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Call(ssa, l, args) => {
                                              write!(f, "CALL   {ssa}, {l}")?;
                for arg in args {
                    write!(f, ", {arg}")?;
                }
                Ok(())
            }
            Instr::Exit =>                    write!(f, "EXIT"),
            Instr::Ret(v) =>                  write!(f, "RET    {v}"),
            Instr::Const(dst, v) =>           write!(f, "CONST  {dst}, {v}"),
            Instr::Alloca(ssa, size, name) => {
                                              write!(f, "ALLOCA {ssa}, {size}")?;
                if let Some(name_hint) = name {
                    write!(f, " (name: {name_hint})")?;
                }
                Ok(())
            }
            Instr::Load(dst, addr) =>         write!(f, "LOAD   {dst}, ({addr})"),
            Instr::Store(src, addr) =>        write!(f, "STORE  {src}, ({addr})"),
            Instr::Print(ssa) =>              write!(f, "PRINT  {ssa}"),
            Instr::Beqz(ssa, bb_zero, bb_nonzero) =>
                                              write!(f, "BEQZ   {ssa}, {bb_zero}, {bb_nonzero}"),
            Instr::BinOp(ssa, operator, lhs, rhs) => {
                let op_name = format!("{operator:?}").to_uppercase();
                                              write!(f, "{op_name:<7}{ssa}, {lhs}, {rhs}", )
            }
            Instr::Goto(blockid, ssas) => {
                                              write!(f, "GOTO   {blockid}")?;
                for ssa in ssas {
                    write!(f, ", {ssa}")?;
                }
                Ok(())
            }
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

        for (i, block) in self.blocks.iter().enumerate() {
            block.pprint(f, i)?;
        }
        Ok(())
    }
}

impl Block {
    pub fn pprint<W: Write>(&self, f: &mut W, i: usize) -> std::io::Result<()> {
        write!(f, "    ^{i}(")?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} {}", param.name, param.ssa)?;
        }
        writeln!(f, ")")?;

        for instr in &self.instrs {
            writeln!(f, "        {instr}");
        }
        Ok(())
    }
}
