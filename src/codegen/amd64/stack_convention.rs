//! Conventions for stack usage.
use super::assembly::*;
use super::x86::*;

use Op::*;
use Operand::*;
use Register::*;

pub trait StackConvention {
    fn add_prologue(block: &mut Block);
    fn add_epilogue(block: &mut Block);

    fn prologue() -> Block {
        let mut block = Block::new();
        Self::add_prologue(&mut block);
        block
    }
    fn epilogue() -> Block {
        let mut block = Block::new();
        Self::add_epilogue(&mut block);
        block
    }
}

pub struct Linux64;
impl StackConvention for Linux64 {
    fn add_prologue(block: &mut Block) {
        block
            .push_cmt(Push, [Reg(Rbp)], "store base pointer")
            .blank();
    }

    fn add_epilogue(block: &mut Block) {
        block
            .blank()
            .push_cmt(Pop, [Reg(Rbp)], "restore previous base pointer")
            .push_cmt(Ret, [], "return to caller");
    }
}

pub struct Windows64;
impl StackConvention for Windows64 {
    fn add_prologue(block: &mut Block) {
        block
            .push_cmt(Push, [Reg(Rbp)], "store base pointer")
            .push_cmt(Mov, [Reg(Rbp), Reg(Rsp)], "move base pointer down")
            .push_cmt(Sub, [Reg(Rsp), Lit(32)], "create shadow space")
            .blank();
    }

    fn add_epilogue(block: &mut Block) {
        block
            .blank()
            .push_cmt(Mov, [Reg(Rsp), Reg(Rbp)], "move stack pointer back up")
            .push_cmt(Pop, [Reg(Rbp)], "restore previous base pointer")
            .push_cmt(Ret, [], "return to caller");
    }
}

pub fn make_procedure<S: Into<String>, C: StackConvention>(name: S) -> Procedure {
    Procedure::new(name.into(), C::prologue(), C::epilogue())
}
