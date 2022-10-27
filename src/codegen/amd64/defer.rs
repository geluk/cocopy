use std::fmt::{self, Debug, Display, Formatter};

use crate::{
    codegen::register_allocation::{Allocator, Destination},
    il::{Label, Name, TargetSize, Variable},
    listing::Position,
    prelude::*,
};

use super::{
    assembly::*,
    op_semantics::{Direction, HasOpSemantics, OpSemantics},
    x86::*,
};

#[derive(Debug)]
pub enum DeferredLine {
    Comment(String),
    Label(Label),
    Block(DeferredBlock),
}
impl Display for DeferredLine {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Comment(cmt) => write!(f, "    ; {cmt}"),
            Self::Label(lbl) => write!(f, "{lbl}:"),
            Self::Block(blk) => write!(f, "{blk}"),
        }
    }
}

#[derive(Debug, Default)]
pub struct DeferredBlock {
    /// The instructions contained within this block.
    pub instructions: Vec<DeferredInstr>,
    /// Indicates that caller-saved registers must be preserved before this
    /// block is executed. This is effectively a shorthand for emitting an
    /// `implicit_write` for every register that is considered to be caller-
    /// saved, except that caller preservation can be smart about writing
    /// them all to the stack together.
    pub caller_preserve: bool,
    /// Indicates that, before this block is executed, the stack must be
    /// aligned in some manner meaningful to the current stack convention.
    /// For instance, on various platforms, the stack must be aligned on a
    /// 16-byte boundary before standard library procedures can be called.
    pub align_stack: bool,
    /// The final instruction in this block should be a return from the
    /// current procedure, cleaning up the stack and returning control to
    /// the caller.
    pub ret: bool,
    /// A lock request signals that a variable must be tied to a certain
    /// register within this block. This could happen as a result of calling
    /// conventions, or special operations that require a variable's value to
    /// be in a certain register. The direction specifies whether the lock
    /// should have happened before the block executes (i.e. it reads the
    /// register), or after it (i.e. it writes the register).
    pub lock_requests: Vec<(DeferredReg, Register, Direction)>,
    /// Signals that a register is read implicitly, i.e. without it being
    /// obvious from the instruction operands. This is mainly useful for
    /// lifetime analysis as it indicates that the variable being read must
    /// still be alive at this point.
    pub implicit_reads: Vec<DeferredReg>,
    /// Signals that a register is being written implicitly, i.e. without it
    /// being obvious from the instruction operands. This suggests that the
    /// register write is not associated with any variable, and has instead
    /// occurred as a result of some other (temporary) computation. The
    /// allocator will make sure no live variable occupies the register when
    /// the write occurs.
    pub implicit_writes: Vec<Register>,
}
impl DeferredBlock {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn implicit_read(&mut self, reg: DeferredReg) {
        self.implicit_reads.push(reg);
    }

    pub fn implicit_write(&mut self, register: Register) {
        self.implicit_writes.push(register);
    }

    pub fn lock_register(&mut self, name: Name, reg: Register, dir: Direction) {
        self.lock_requests
            .push((DeferredReg::from_name(name), reg, dir));
    }

    pub fn emit<V: Into<Vec<DeferredOperand>>>(&mut self, op: Op, operands: V) {
        let instr = DeferredInstr {
            op,
            operands: operands.into(),
        };
        debug!("Emit {instr:?}");
        self.instructions.push(instr);
    }
}
impl Display for DeferredBlock {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // Self::LockRequest(dfr, reg) => write!(f, "    lock {dfr} to {reg}"),
        // Self::ImplicitRead(dfr) => write!(f, "    implicit_read {dfr}"),
        // Self::ImplicitWrite(reg) => write!(f, "    implicit_write {reg}"),
        // Self::CallerPreserve => f.write_str("    caller_preserve"),
        // Self::CallerRestore => f.write_str("    caller_restore"),
        // Self::AlignStack => f.write_str("    align_stack"),
        // Self::Return => f.write_str("    return"),
        for instr in &self.instructions {
            write!(f, "{instr}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct DeferredInstr {
    pub op: Op,
    pub operands: Vec<DeferredOperand>,
}
impl Display for DeferredInstr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "    {} {}",
            self.op,
            self.operands
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone)]
pub enum DeferredOperand {
    /// A fixed register that is identified ahead of time, likely as a result of
    /// calling conventions or operator semantics.
    ConstReg(Register),
    /// Some not yet known register.
    /// Based on the operator semantics of the operator to which this operand
    /// belongs, the register allocator can make sure to assign the name to a
    /// valid register, or to emit a just-in-time swap to bring it into the
    /// right register.
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
        op: Op,
        index: usize,
    ) -> Operand {
        match self {
            Self::ConstReg(reg) => Operand::Reg(reg),
            Self::Reg(deferred) => {
                let op_semantics = op.op_semantics(index);
                match op_semantics.direction() {
                    Direction::Read => deferred.resolve(allocator, position.read(), op_semantics),
                    Direction::Write => deferred.resolve(allocator, position.write(), op_semantics),
                }
            }
            Self::Lit(lit) => Operand::Lit(lit as i128),
            Self::Lbl(lbl) => Operand::Lbl(format!(".{lbl}")),
            Self::Id(id) => Operand::Id(id),
        }
    }

    pub fn from_name(name: Name) -> Self {
        Self::Reg(DeferredReg::from_name(name))
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
