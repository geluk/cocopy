//! Native code generation for 64-bit Linux.

use crate::codegen::amd64::calling_convention::CallingConvention;
use crate::codegen::amd64::procedure_compiler::ProcedureCompiler;
use crate::il::TacProgram;

use super::assembly::*;
use super::x86::*;

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(prog: TacProgram) -> Assembly {
    use Decl::*;
    let mut asm = make_assembly();

    asm.push_decl(Bits(64))
        .push_decl(Extern("printf"))
        .push_decl(Global("main"));

    asm.text.main =
        ProcedureCompiler::compile(prog.top_level, asm.text.main, CallingConvention::SystemV64);

    asm.text.main.body.blank().push(Mov, [Reg(Rax), Lit(0)]);

    asm.text.procedures.push(print());

    asm.data.db("msg_i", "'The integer is %i', 10, 0");

    asm
}

fn print() -> Procedure {
    let mut print = procedure("print");
    print
        .body
        .push(Mov, [Reg(Rsi), Reg(Rdi)])
        .push(Lea, [Reg(Rdi), Id("[msg_i]".to_string())])
        .push(Call, [Id("printf".to_string())]);
    print
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
