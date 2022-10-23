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
#[derive(Debug, Default, Clone, Copy)]
pub struct OpSemantics {
    immediate: bool,
    register: Option<RegSemantics>,
}
impl OpSemantics {
    /// The operand may be placed in memory or in any register.
    fn any_reg_or_mem() -> Self {
        Self {
            register: Some(RegSemantics::any()),
            // memory: true,
            ..Default::default()
        }
    }
    /// The operand may be placed in any register.
    fn any_reg() -> Self {
        Self {
            register: Some(RegSemantics::any()),
            ..Default::default()
        }
    }
    fn any() -> Self {
        Self {
            register: Some(RegSemantics::any()),
            immediate: true,
        }
    }

    pub fn immediate(&self) -> bool {
        self.immediate
    }

    pub fn register(&self) -> Option<RegSemantics> {
        self.register
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

/// TODO: Op semantics are more complicated than this and should be modelled
/// as a list of rules.
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

use Op::*;
impl HasOpSemantics for Op {
    /// Retrieves the operand semantics for the first operand.
    fn op1_semantics(&self) -> OpSemantics {
        match self {
            Test => OpSemantics::any_reg(),
            Cmp => OpSemantics::any_reg_or_mem(),
            Idiv => OpSemantics::any_reg_or_mem(),
            Mov => OpSemantics::any(),
            Sete | Setg | Setge | Setl | Setle | Setne => OpSemantics {
                immediate: false,
                register: Some(RegSemantics::any_sized(PtrSize::Byte)),
            },
            _ => {
                warn!(
                    "Don't know operand 1 semantics for {}, guessing 'any'",
                    self
                );
                // Not a big problem if this is wrong, because then nasm will just throw an error
                // if we try to assemble the code.
                OpSemantics::any()
            }
        }
    }
    /// Retrieves the operand semantics for the second operand.
    fn op2_semantics(&self) -> OpSemantics {
        match self {
            Cmp => OpSemantics::any_reg(),
            Mov => OpSemantics::any(),
            _ => {
                warn!(
                    "Don't know operand 2 semantics for {}, guessing 'any'",
                    self
                );
                // Not a big problem if this is wrong, because then nasm will just throw an error
                // if we try to assemble the code.
                OpSemantics::any()
            }
        }
    }
}
