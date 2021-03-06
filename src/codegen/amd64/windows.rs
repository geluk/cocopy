//! Native code generation for 64-bit Windows.

use crate::il::*;

use super::{
    assembly::*,
    calling_convention::CallingConvention,
    procedure_compiler::ProcedureCompiler,
    stack_convention::{self, StackConvention, Windows64},
    x86::*,
};

use Op::*;
use Operand::*;
use Register::*;

pub fn compile(prog: TacProgram) -> Assembly {
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
        .push(Op::Call, [Id("_CRT_INIT".to_string())])
        .blank();

    asm.text.main = ProcedureCompiler::<Windows64>::compile(
        prog.top_level,
        asm.text.main,
        CallingConvention::Microsoft64,
    );

    for (name, listing) in prog.functions {
        let proc = ProcedureCompiler::<Windows64>::compile(
            listing,
            procedure(name),
            CallingConvention::Microsoft64,
        );
        asm.text.procedures.push(proc);
    }

    asm.text
        .main
        .body
        .blank()
        .ret_zero()
        .push(Call, [Id("ExitProcess".to_string())]);

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
        .push(Mov, [Reg(Rdx), Reg(Rcx)])
        .push(Lea, [Reg(Rcx), Id("[msg_i]".to_string())])
        .push(Call, [Id("printf".to_string())]);
    print
}

fn make_assembly() -> Assembly {
    Assembly::new(Procedure::new(
        "main".to_string(),
        Windows64::prologue(),
        Block::new(),
    ))
}

fn procedure<S: Into<String>>(name: S) -> Procedure {
    stack_convention::make_procedure::<_, Windows64>(name)
}

trait BodyExt {
    fn ret_zero(&mut self) -> &mut Self;
}
impl BodyExt for Block {
    fn ret_zero(&mut self) -> &mut Self {
        self.push_cmt(Xor, [Reg(Rax), Reg(Rax)], "return zero");
        self
    }
}
