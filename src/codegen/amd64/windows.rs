//! Native code generation for 64-bit Windows.

use crate::il::*;

use super::assembly::*;

pub fn compile(_prog: &Vec<Instruction>) -> Assembly {
    default()
}

fn default() -> Assembly {
    use Decl::*;
    let mut asm = Assembly::new();

    asm.push_decl(Bits(64))
        .push_decl(Default("rel"))
        .push_decl(Global("main"))
        .push_decl(Extern("_CRT_INIT"))
        .push_decl(Extern("ExitProcess"))
        .push_decl(Extern("printf"));

    asm.text
        .label("main")
        .prologue()
        .ins("call", "_CRT_INIT")
        .blank();

    asm.text
        .ins("lea", "rcx, [msg_ch]")
        .ins("mov", "rdx, [msg_ch]")
        .ins("call", "printf")
        .blank();

    asm.text
        .ins("mov", "rcx, 12")
        .ins("call", "square")
        .ins("lea", "rcx, [msg_i]")
        .ins("mov", "rdx, rax")
        .ins("call", "printf")
        .blank();

    asm.text.ret_zero().ins("call", "ExitProcess").blank();

    asm.text
        .label("square")
        .prologue()
        .ins("mov", "rax, rcx")
        .ins("mul", "rax")
        .epilogue();

    asm.data
        .ins("msg_ch", "db 'The char is %c', 13, 10, 0")
        .ins("msg_i", "db 'The integer is %i', 13, 10, 0'");

    asm
}

pub trait ProcedureExt {
    fn prologue(&mut self) -> &mut Self;
    fn epilogue(&mut self) -> &mut Self;
    fn ret_zero(&mut self) -> &mut Self;
}
impl ProcedureExt for Section {
    fn prologue(&mut self) -> &mut Self {
        self.ins_cmt("push", "rbp", "Store base pointer")
            .ins_cmt("mov", "rbp, rsp", "Move base pointer down")
            .ins_cmt("sub", "rsp, 32", "Create shadow space")
            .blank()
    }

    fn epilogue(&mut self) -> &mut Self {
        self.blank()
            .ins_cmt("mov", "rsp, rbp", "Move stack pointer back up")
            .ins_cmt("pop", "rbp", "Restore previous base pointer")
            .ins_cmt("ret", "", "Return to caller")
            .blank()
    }

    fn ret_zero(&mut self) -> &mut Self {
        self.ins_cmt("xor", "rax, rax", "Return zero")
    }
}
