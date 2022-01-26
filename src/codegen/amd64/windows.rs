//! Native code generation for 64-bit Windows.

use crate::il::*;

use super::{assembly::*, procedure_compiler::ProcedureCompiler, x86::*};

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(prog: TacListing) -> Assembly {
    use Decl::*;
    let mut asm = make_assembly();

    asm.push_decl(Bits(64))
        .push_decl(Default("rel"))
        .push_decl(Global("main"))
        .push_decl(Extern("_CRT_INIT"))
        .push_decl(Extern("ExitProcess"))
        .push_decl(Extern("printf"));

    asm.text
        .main
        .body
        .push(Op::Call, vec![Id("_CRT_INIT")])
        .blank();

    asm.text.main = ProcedureCompiler::compile(prog, asm.text.main);

    asm.text
        .main
        .body
        .blank()
        .ret_zero()
        .push(Call, vec![Id("ExitProcess")]);

    asm.text.procedures.push(print());

    asm.data
        .db("msg_ch", "'The char is %c', 13, 10, 0")
        .db("msg_i", "'The integer is %i', 13, 10, 0");

    asm
}

fn print() -> Procedure {
    let mut print = procedure("print");
    print
        .body
        .push(Mov, vec![Reg(Rdx), Reg(Rcx)])
        .push(Lea, vec![Reg(Rcx), Id("[msg_i]")])
        .push(Call, vec![Id("printf")]);
    print
}

fn make_assembly() -> Assembly {
    Assembly::new(Procedure::new("main", prologue(), Block::new()))
}

fn procedure(name: Str) -> Procedure {
    Procedure::new(name, prologue(), epilogue())
}

fn prologue() -> Block {
    let mut prologue = Block::new();

    prologue
        .push_cmt(Push, vec![Reg(Rbp)], "Store base pointer")
        .push_cmt(Mov, vec![Reg(Rbp), Reg(Rsp)], "Move base pointer down")
        .push_cmt(Sub, vec![Reg(Rsp), Lit(32)], "Create shadow space")
        .blank();

    prologue
}

fn epilogue() -> Block {
    let mut epilogue = Block::new();
    epilogue
        .blank()
        .push_cmt(Mov, vec![Reg(Rsp), Reg(Rbp)], "Move stack pointer back up")
        .push_cmt(Pop, vec![Reg(Rbp)], "Restore previous base pointer")
        .push_cmt(Ret, vec![], "Return to caller");

    epilogue
}

trait BodyExt {
    fn ret_zero(&mut self) -> &mut Self;
}
impl BodyExt for Block {
    fn ret_zero(&mut self) -> &mut Self {
        self.push_cmt(Xor, vec![Reg(Rax), Reg(Rax)], "Return zero");
        self
    }
}
