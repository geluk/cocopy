use crate::prelude::*;

use super::{assembly::PtrSize, x86::Op};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AcceptsReg {
    AnyReg,
}
impl Default for AcceptsReg {
    fn default() -> Self {
        Self::AnyReg
    }
}

/// Describes what types of locations are accepted for an operand.
#[derive(Debug, Clone, Copy)]
pub struct OpSemantics {
    immediate: bool,
    register: Option<RegSemantics>,
    direction: Direction,
}
impl OpSemantics {
    pub fn immediate(&self) -> bool {
        self.immediate
    }

    pub fn register(&self) -> Option<RegSemantics> {
        self.register
    }

    pub fn direction(&self) -> Direction {
        self.direction
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct RegSemantics {
    register: AcceptsReg,
    requires_size: Option<PtrSize>,
}
impl RegSemantics {
    pub fn any() -> Self {
        Self {
            register: AcceptsReg::AnyReg,
            requires_size: None,
        }
    }

    pub fn any_sized(size: PtrSize) -> RegSemantics {
        Self {
            register: AcceptsReg::AnyReg,
            requires_size: Some(size),
        }
    }

    pub fn requires_size(&self) -> Option<PtrSize> {
        self.requires_size
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    Read,
    Write,
}

pub trait HasOpSemantics {
    /// Get the operand semantics for the first operand of this instruction.
    fn op1_semantics(&self) -> OpSemantics;
    /// Get the operand semantics for the second operand of this instruction.
    fn op2_semantics(&self) -> OpSemantics;

    fn op_semantics(&self, index: usize) -> OpSemantics {
        match index {
            0 => self.op1_semantics(),
            1 => self.op2_semantics(),
            n => {
                panic!(
                    "Attempted to determine semantics for invalid {}-operand assembly instruction",
                    n + 1
                )
            }
        }
    }
}

use Direction::*;
use Op::*;
impl HasOpSemantics for Op {
    /// Retrieves the operand semantics for the first operand.
    fn op1_semantics(&self) -> OpSemantics {
        match self {
            Test => direction(Read).any_register(),
            Cmp => direction(Read).any_register(),
            Mov => direction(Write).any_register().immediate(),
            Sete | Setg | Setge | Setl | Setle | Setne => {
                direction(Write).any_sized_register(PtrSize::Byte)
            }
            _ => {
                warn!(
                    "Don't know operand 1 semantics for {}, guessing 'write + any'",
                    self
                );
                // Not a big problem if the destination is wrong, because then nasm will just throw an error
                // if we try to assemble the code. The direction is trickier though.
                direction(Write).any()
            }
        }
        .build()
    }
    /// Retrieves the operand semantics for the second operand.
    fn op2_semantics(&self) -> OpSemantics {
        match self {
            Cmp => direction(Read).any_register(),
            Mov => direction(Read).any(),
            _ => {
                warn!(
                    "Don't know operand 2 semantics for {}, guessing 'any'",
                    self
                );
                // Not a big problem if the destination is wrong, because then nasm will just throw an error
                // if we try to assemble the code. The direction is trickier though.
                direction(Read).any()
            }
        }
        .build()
    }
}

fn direction(direction: Direction) -> Builder {
    Builder::new(direction)
}

struct Builder {
    direction: Direction,
    register: Option<RegSemantics>,
    immediate: bool,
}
impl Builder {
    pub fn new(direction: Direction) -> Self {
        Self {
            direction,
            register: None,
            immediate: false,
        }
    }

    pub fn any_register(mut self) -> Self {
        self.register = Some(RegSemantics::any());
        self
    }

    pub fn any_sized_register(mut self, size: PtrSize) -> Self {
        self.register = Some(RegSemantics::any_sized(size));
        self
    }

    pub fn immediate(mut self) -> Self {
        self.immediate = true;
        self
    }

    pub fn any(self) -> Self {
        self.immediate().any_register()
    }

    pub fn build(self) -> OpSemantics {
        OpSemantics {
            direction: self.direction,
            register: self.register,
            immediate: self.immediate,
        }
    }
}
