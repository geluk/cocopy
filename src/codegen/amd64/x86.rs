use std::{
    fmt::{self, Display, Formatter},
    slice::Iter,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    RBP,
    RSP,
    RDI,
    RSI,
    RAX,
    RBX,
    RCX,
    RDX,
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
        [RAX, RBX, RCX, RDX, R8, R9, R10, R11, R12, R13, R14, R15].iter()
    }
}
impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Register::RBP => f.write_str("rbp"),
            Register::RSP => f.write_str("rsp"),
            Register::RDI => f.write_str("rdi"),
            Register::RSI => f.write_str("rsi"),
            Register::RAX => f.write_str("rax"),
            Register::RBX => f.write_str("rbx"),
            Register::RCX => f.write_str("rcx"),
            Register::RDX => f.write_str("rdx"),
            Register::R8 => f.write_str("r8"),
            Register::R9 => f.write_str("r9"),
            Register::R10 => f.write_str("r10"),
            Register::R11 => f.write_str("r11"),
            Register::R12 => f.write_str("r12"),
            Register::R13 => f.write_str("r13"),
            Register::R14 => f.write_str("r14"),
            Register::R15 => f.write_str("r15"),
        }
    }
}

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
    Sub,
    Imul,
    Add,
    // Bitwise operations
    Xor,
}
impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Op::Push => f.write_str("push"),
            Op::Pop => f.write_str("pop"),
            Op::Call => f.write_str("call"),
            Op::Ret => f.write_str("ret"),
            Op::Mov => f.write_str("mov"),
            Op::Lea => f.write_str("lea"),
            Op::Sub => f.write_str("sub"),
            Op::Imul => f.write_str("imul"),
            Op::Add => f.write_str("add"),
            Op::Xor => f.write_str("xor"),
        }
    }
}
