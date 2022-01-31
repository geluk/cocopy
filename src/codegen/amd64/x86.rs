use std::{
    fmt::{self, Display, Formatter},
    slice::Iter,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
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
}
impl Register {
    pub fn iter() -> Iter<'static, Register> {
        use Register::*;
        [Rax, Rbx, Rcx, Rdx, R8, R9, R10, R11, R12, R13, R14, R15].iter()
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
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code, reason = "We prefer completeness here")]
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
    // Bitwise operations
    Xor,
    // Comparison
    Cmp,
    Test,
    // Jumps
    Jz,
    Jnz,
    Je,
    Jne,
    Jg,
    Jge,
    Jl,
    Jle,
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
            Op::Xor => "xor",
            Op::Cmp => "cmp",
            Op::Test => "test",
            Op::Jz => "jz",
            Op::Jnz => "jnz",
            Op::Je => "je",
            Op::Jne => "jne",
            Op::Jg => "jg",
            Op::Jge => "jge",
            Op::Jl => "jl",
            Op::Jle => "jle",
            Op::Nop => "nop",
        })
    }
}
