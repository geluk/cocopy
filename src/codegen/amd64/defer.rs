use std::fmt::{self, Debug, Display, Formatter};

use crate::{
    codegen::register_allocation::{Allocator, Destination},
    il::*,
    listing::Position,
};

use super::{assembly::*, procedure_compiler::OpSemantics, x86::*};

#[derive(Debug)]
pub enum DeferredLine {
    Comment(String),
    Label(Label),
    Instr(DeferredInstr),
    LockRequest(DeferredReg, Register),
    CallerPreserve,
    CallerRestore,
    AlignStack,
    Return,
}
impl Display for DeferredLine {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Comment(cmt) => write!(f, "# {cmt}"),
            Self::Label(lbl) => write!(f, "{lbl}:"),
            Self::Instr(instr) => write!(
                f,
                "{} {}",
                instr.op,
                instr
                    .target
                    .iter()
                    .map(|t| t.to_string())
                    .chain(instr.operands.iter().map(|i| i.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::LockRequest(dfr, reg) => write!(f, "lock {dfr:?} to {reg}"),
            Self::CallerPreserve => f.write_str("caller_preserve"),
            Self::CallerRestore => f.write_str("caller_restore"),
            Self::AlignStack => f.write_str("align_stack"),
            Self::Return => f.write_str("return"),
        }
    }
}

#[derive(Debug)]
pub struct DeferredInstr {
    pub op: Op,
    pub target: Option<Target>,
    pub operands: Vec<DeferredOperand>,
}

#[derive(Debug, Clone)]
pub enum DeferredOperand {
    /// Some not yet known register.
    /// Also includes the operator semantics so that the register allocator
    /// can make sure to assign the name to a valid register, or to emit a
    /// just-in-time swap to bring it into the right register.
    Reg(DeferredReg, OpSemantics),
    /// An immediate value
    Lit(TargetSize),
    /// A label
    Lbl(Label),
    /// An identifier
    Id(String),
}
impl DeferredOperand {
    /// Given an allocator and a source code position, resolve the deferred
    /// operand back to an assembly operand.
    pub fn resolve(
        self,
        allocator: &Allocator<DeferredReg, Register>,
        position: Position,
    ) -> Operand {
        match self {
            // TODO: Improve position handling to make it clear what's happening here
            Self::Reg(deferred, _) => deferred.resolve(allocator, position - 1),
            Self::Lit(lit) => Operand::Lit(lit as i128),
            Self::Lbl(lbl) => Operand::Lbl(format!(".{lbl}")),
            Self::Id(id) => Operand::Id(id),
        }
    }
}
impl Display for DeferredOperand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Reg(reg, _) => write!(f, "{}", reg),
            Self::Lit(lit) => write!(f, "{}", lit),
            Self::Lbl(lbl) => write!(f, "{}", lbl),
            Self::Id(id) => write!(f, "{}", id),
        }
    }
}

/// An as of yet unknown register. After the register allocation pass, this
/// can be definitively resolved to a register by querying the register
/// allocator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeferredReg {
    Sub(Variable),
    Temp(usize),
    AsmTemp(usize),
}
impl DeferredReg {
    /// Given an allocator and a source code position, resolve the deferred
    /// register back to an assembly operand.
    pub fn resolve(
        self,
        allocator: &Allocator<DeferredReg, Register>,
        position: Position,
    ) -> Operand {
        match allocator.lookup(&self, position) {
            Destination::Reg(alloc) => Operand::Reg(alloc.register()),
            Destination::Stack(_) => todo!("Reference the stack"),
        }
    }
}
impl Display for DeferredReg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            DeferredReg::Sub(sub) => write!(f, "{}", sub),
            DeferredReg::Temp(temp) => write!(f, "%{}", temp),
            DeferredReg::AsmTemp(temp) => write!(f, "${}", temp),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Target {
    Deferred(DeferredReg),
    Reg(Register),
}
impl Display for Target {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Target::Deferred(dfr) => write!(f, "{}", dfr),
            Target::Reg(reg) => write!(f, "{}", reg),
        }
    }
}
