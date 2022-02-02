//! Native code generation for 64-bit Linux.

use crate::il::TacListing;

use super::assembly::*;
use super::x86::*;

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(_prog: TacListing) -> Assembly {
    default()
}

fn default() -> Assembly {
    use Decl::*;
    let mut asm = make_assembly();

    asm.push_decl(Extern("printf")).push_decl(Global("main"));

    asm.text
        .main
        .body
        .push(Mov, [Reg(Rdi), Id("fmt_int")])
        .push(Mov, [Reg(Rsi), Lit(101)])
        .push(Call, [Id("printf")])
        .push(Mov, [Reg(Rax), Lit(0)]);

    asm.data.db("fmt_int", "'%i', 0");

    asm
}

fn make_assembly() -> Assembly {
    Assembly::new(procedure("main"))
}

fn procedure(name: Str) -> Procedure {
    Procedure::new(name, prologue(), epilogue())
}

fn prologue() -> Block {
    let mut prologue = Block::new();
    prologue.push_cmt(Push, [Reg(Rbp)], "store base pointer");

    prologue
}

fn epilogue() -> Block {
    let mut epilogue = Block::new();
    epilogue
        .push_cmt(Pop, [Reg(Rbp)], "restore previous base pointer")
        .push_cmt(Ret, [], "return to caller");

    epilogue
}
