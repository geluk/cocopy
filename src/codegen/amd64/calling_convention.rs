use std::{
    iter::{Copied, Enumerate},
    slice::Iter,
};

use super::x86::Register::{self, *};

const MICROSOFT_PARAMS: &[Register] = &[Rcx, Rdx, R8, R9];
const SYSTEM_V_PARAMS: &[Register] = &[Rdi, Rsi, Rdx, Rcx, R8, R9];

const SYSTEM_V_CALLEE_SAVE: &[Register] = &[Rbx, Rsp, Rbp, R12, R13, R14, R15];
const MICROSOFT_CALLEE_SAVE: &[Register] = &[Rbx, Rsp, Rbp, Rsi, Rdi, R12, R13, R14, R15];

#[derive(Debug)]
pub enum CallingConvention {
    /// The Microsoft x64 calling convention
    Microsoft64,
    /// The System V AMD64 ABI
    SystemV64,
}

impl CallingConvention {
    /// Iterate over the parameters allowed by this calling convention.
    pub fn iter_params(&self, param_count: usize) -> Enumerate<Copied<Iter<Register>>> {
        let max_params = self.reg_param_count();
        assert!(
            param_count <= max_params,
            "Passing more than {} parameters is not supported yet",
            max_params
        );
        self.get_params()[0..param_count]
            .iter()
            .copied()
            .enumerate()
    }

    /// Returns `true` if `register` is callee-saved, meaning that a function
    /// should preserve this register before using it.
    pub fn is_callee_saved(&self, register: Register) -> bool {
        self.get_callee_saved_regs().contains(&register)
    }

    /// Returns `true` if `register` is caller-saved, meaning that a function
    /// should preserve this register if it needs access to its value
    /// after it has called a child function.
    pub fn is_caller_saved(&self, register: Register) -> bool {
        !self.is_callee_saved(register)
    }

    /// Gets the registers that are callee-saved, meaning that a function
    /// should preserve these registers before using them.
    pub fn get_callee_saved_regs(&self) -> &'static [Register] {
        match self {
            CallingConvention::Microsoft64 => MICROSOFT_CALLEE_SAVE,
            CallingConvention::SystemV64 => SYSTEM_V_CALLEE_SAVE,
        }
    }

    /// Gets the register used for passing return values back to the caller.
    pub fn get_return_reg(&self) -> Register {
        // As long as we only deal with 64-bit wide return values,
        // this is the same for Microsoft and SystemV.
        Rax
    }

    /// Gets the maximum amount of parameters that can be passed via registers.
    pub fn reg_param_count(&self) -> usize {
        self.get_params().len()
    }

    /// Gets the registers that are used for parameters.
    pub fn get_params(&self) -> &'static [Register] {
        match self {
            CallingConvention::Microsoft64 => MICROSOFT_PARAMS,
            CallingConvention::SystemV64 => SYSTEM_V_PARAMS,
        }
    }

    /// Gets the size of the required stack alignment boundary.
    pub fn stack_alignment(&self) -> usize {
        match self {
            CallingConvention::Microsoft64 => 16,
            CallingConvention::SystemV64 => 16,
        }
    }
}
