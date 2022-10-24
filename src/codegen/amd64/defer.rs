use std::fmt::{self, Debug, Display, Formatter};

use crate::{
    codegen::register_allocation::{Allocator, Destination},
    il::{Label, Name, TargetSize, Variable},
    listing::Position,
};

use super::{
    assembly::*,
    op_semantics::{Direction, OpSemantics},
    x86::*,
};

#[derive(Debug)]
pub enum DeferredLine {
    Comment(String),
    Label(Label),
    Instr(DeferredInstr),
    LockRequest(DeferredReg, Register),
    ImplicitRead(DeferredReg),
    ImplicitWrite(Register),
    CallerPreserve,
    CallerRestore,
    AlignStack,
    Return,
}
impl Display for DeferredLine {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Comment(cmt) => write!(f, "    ; {cmt}"),
            Self::Label(lbl) => write!(f, "{lbl}:"),
            Self::Instr(instr) => write!(
                f,
                "    {} {}",
                instr.op,
                instr
                    .operands
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::LockRequest(dfr, reg) => write!(f, "    lock {dfr} to {reg}"),
            Self::ImplicitRead(dfr) => write!(f, "    implicit_read {dfr}"),
            Self::ImplicitWrite(reg) => write!(f, "    implicit_write {reg}"),
            Self::CallerPreserve => f.write_str("    caller_preserve"),
            Self::CallerRestore => f.write_str("    caller_restore"),
            Self::AlignStack => f.write_str("    align_stack"),
            Self::Return => f.write_str("    return"),
        }
    }
}

#[derive(Debug)]
pub struct DeferredInstr {
    pub op: Op,
    pub operands: Vec<DeferredOperand>,
}

#[derive(Debug, Clone)]
pub enum DeferredOperand {
    /// A fixed register that is identified ahead of time, likely as a result of
    /// calling conventions or operator semantics.
    ConstReg(Register),
    /// Some not yet known register.
    /// Also includes the operator semantics so that the register allocator
    /// can make sure to assign the name to a valid register, or to emit a
    /// just-in-time swap to bring it into the right register.
    Reg(DeferredReg),
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
        op_semantics: OpSemantics,
    ) -> Operand {
        match self {
            Self::ConstReg(reg) => Operand::Reg(reg),
            Self::Reg(deferred) => match op_semantics.direction() {
                Direction::Read => deferred.resolve(allocator, position.read(), op_semantics),
                Direction::Write => deferred.resolve(allocator, position.write(), op_semantics),
            },
            Self::Lit(lit) => Operand::Lit(lit as i128),
            Self::Lbl(lbl) => Operand::Lbl(format!(".{lbl}")),
            Self::Id(id) => Operand::Id(id),
        }
    }
}
impl Display for DeferredOperand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::ConstReg(reg) => write!(f, "{}", reg),
            Self::Reg(reg) => write!(f, "{}", reg),
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
    /// Convert a [`Name`] to a deferred register.
    pub fn from_name(name: Name) -> DeferredReg {
        match name {
            Name::Sub(sub) => Self::Sub(sub),
            Name::Temp(temp) => Self::Temp(temp),
        }
    }
    /// Given an allocator and a source code position, resolve the deferred
    /// register back to an assembly operand.
    pub fn resolve(
        self,
        allocator: &Allocator<DeferredReg, Register>,
        position: Position,
        op_semantics: OpSemantics,
    ) -> Operand {
        let size = op_semantics.register().and_then(|r| r.requires_size());
        match (allocator.lookup(&self, position), size) {
            (Destination::Reg(alloc), None) => Operand::Reg(alloc.register()),
            (Destination::Reg(alloc), Some(size)) => Operand::Reg(alloc.register().resize(size)),
            (Destination::Stack(_), _) => todo!("Reference the stack"),
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

/// Helper trait for instructions that consider reads and writes to happen on different lines.
pub trait ReadWriteSemantics {
    /// Offsets the position for a read operation.
    fn read(&self) -> Self;
    /// Offsets the position for a write operation.
    fn write(&self) -> Self;
}

impl ReadWriteSemantics for Position {
    fn read(&self) -> Self {
        // Reads are considered to happen on the same line.
        *self
    }

    fn write(&self) -> Self {
        // Writes are considered to happen on the next line.
        *self + 1
    }
}
