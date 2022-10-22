#![allow(dead_code, reason = "We prefer completeness here")]
use std::{
    fmt::{self, Display, Formatter},
    slice::Iter,
};

/// An x86 register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    // 64-bit wide registers
    Rbp,
    Rsp,
    Rdi,
    Rsi,
    Rax,
    Rbx,
    Rcx,
    Rdx,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // 32-bit wide registers
    Eax,
    // 8-bit wide registers
    Al,
    Bl,
    Cl,
    Dl,
    R8b,
    R9b,
    R10b,
    R11b,
    R12b,
    R13b,
    R14b,
    R15b,
}
impl Register {
    pub fn iter() -> Iter<'static, Register> {
        use Register::*;
        [Rax, Rbx, Rcx, Rdx, R8, R9, R10, R11, R12, R13, R14, R15].iter()
    }

    pub fn into_byte(self) -> Self {
        use Register::*;
        match self {
            Rax => Al,
            Rbx => Bl,
            Rcx => Cl,
            Rdx => Dl,
            R8 => R8b,
            R9 => R9b,
            R10 => R10b,
            R11 => R11b,
            R12 => R12b,
            R13 => R13b,
            R14 => R14b,
            R15 => R15b,
            other => panic!("Can't convert {} into a byte register", other),
        }
    }

    pub fn byte_size(self) -> usize {
        use Register::*;
        match self {
            Rbp | Rsp | Rdi | Rsi | Rax | Rbx | Rcx | Rdx | R8 | R9 | R10 | R11 | R12 | R13
            | R14 | R15 => 8,
            _ => {
                unimplemented!("Consider the implications of handling variable-size registers.")
            }
        }
    }
}
impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Register::Rbp => "rbp",
            Register::Rsp => "rsp",
            Register::Rdi => "rdi",
            Register::Rsi => "rsi",
            Register::Rax => "rax",
            Register::Eax => "eax",
            Register::Rbx => "rbx",
            Register::Rcx => "rcx",
            Register::Rdx => "rdx",
            Register::R8 => "r8",
            Register::R9 => "r9",
            Register::R10 => "r10",
            Register::R11 => "r11",
            Register::R12 => "r12",
            Register::R13 => "r13",
            Register::R14 => "r14",
            Register::R15 => "r15",
            Register::Al => "al",
            Register::Bl => "bl",
            Register::Cl => "cl",
            Register::Dl => "dl",
            Register::R8b => "r8b",
            Register::R9b => "r9b",
            Register::R10b => "r10b",
            Register::R11b => "r11b",
            Register::R12b => "r12b",
            Register::R13b => "r13b",
            Register::R14b => "r14b",
            Register::R15b => "r15b",
        })
    }
}

/// An x86 instruction (called `Op` to prevent confusion with TAC instructions).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    // Stack manipulation
    Push,
    Pop,
    Call,
    Ret,
    // Copies
    Mov,
    Lea,
    // Arithmetic
    Add,
    Sub,
    Imul,
    Idiv,
    // Bitwise operations
    Xor,
    // Comparison
    Cmp,
    Test,
    // Jumps
    Jmp,
    Jz,
    Jnz,
    Je,
    Jne,
    Jg,
    Jge,
    Jl,
    Jle,
    // Conditional set
    Setg,
    Setge,
    Setl,
    Setle,
    Sete,
    Setne,
    // Misc
    Nop,
}
impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Op::Push => "push",
            Op::Pop => "pop",
            Op::Call => "call",
            Op::Ret => "ret",
            Op::Mov => "mov",
            Op::Lea => "lea",
            Op::Add => "add",
            Op::Sub => "sub",
            Op::Imul => "imul",
            Op::Idiv => "idiv",
            Op::Xor => "xor",
            Op::Cmp => "cmp",
            Op::Test => "test",
            Op::Jmp => "jmp",
            Op::Jz => "jz",
            Op::Jnz => "jnz",
            Op::Je => "je",
            Op::Jne => "jne",
            Op::Jg => "jg",
            Op::Jge => "jge",
            Op::Jl => "jl",
            Op::Jle => "jle",
            Op::Nop => "nop",
            Op::Setg => "setg",
            Op::Setge => "setge",
            Op::Setl => "setl",
            Op::Setle => "setle",
            Op::Sete => "sete",
            Op::Setne => "setne",
        })
    }
}
