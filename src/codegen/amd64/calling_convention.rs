use std::{
    iter::{Copied, Enumerate},
    slice::Iter,
};

use super::x86::Register::{self, *};

const MICROSOFT_PARAMS: &[Register] = &[Rcx, Rdx, R8, R9];
const SYSTEM_V_PARAMS: &[Register] = &[Rdi, Rsi, Rdx, Rcx, R8, R9];

pub enum CallingConvention {
    /// The Microsoft x64 calling convention
    Microsoft64,
    /// The System V AMD64 ABI
    SystemV64,
}

impl CallingConvention {
    /// Iterate over the parameters allowed by this calling convention
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

    /// Gets the maximum amount of parameters that can be passed via registers.
    pub fn reg_param_count(&self) -> usize {
        self.get_params().len()
    }

    pub fn get_params(&self) -> &'static [Register] {
        match self {
            CallingConvention::Microsoft64 => MICROSOFT_PARAMS,
            CallingConvention::SystemV64 => SYSTEM_V_PARAMS,
        }
    }
}
