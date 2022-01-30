
use crate::{ast::untyped::BinOp, builtins::Builtin, il::*};

use super::{assembly::*, register_allocator::RegisterAllocator, x86::*};

use Op::*;
use Operand::*;
use Register::*;

struct StackOffset(usize);

pub struct ProcedureCompiler {
    allocator: RegisterAllocator,
    procedure: Procedure,
    param_stack: Vec<Operand>,
    spills: Vec<Name>,
}
impl ProcedureCompiler {
    pub fn compile(listing: TacListing, procedure: Procedure) -> Procedure {
        let mut compiler = Self {
            allocator: RegisterAllocator::new(),
            procedure,
            param_stack: vec![],
            spills: Vec::new(),
        };
        compiler.compile_tac(listing);

        compiler.procedure
    }

    pub fn compile_tac(&mut self, listing: TacListing) {
        let mut final_tgt = None;
        for (_, instr) in listing.iter_lines() {
            let oper = self.compile_instr(instr);
            // TODO: After each instruction has been compiled, iterate over allocated registers,
            // and free those whose value will no longer be used.
            final_tgt.replace(oper);
        }
    }

    fn compile_instr(&mut self, instr: &Instruction) {
        let comment = instr.to_string();
        match &instr.kind {
            InstrKind::Assign(tgt, value) => {
                self.compile_assign(tgt.clone(), value.clone(), comment)
            }
            InstrKind::Bin(tgt, op, left, right) => {
                self.compile_bin(tgt.clone(), *op, left.clone(), right.clone(), comment)
            }
            InstrKind::Param(param) => self.compile_param(param.clone()),
            InstrKind::Call(tgt, name, params) => self.compile_call(tgt.clone(), *name, *params),
        }
    }

    fn compile_assign(&mut self, target: Name, value: Value, comment: String) {
        let target = self.lookup(target);
        let value = self.get_operand(value);

        self.procedure
            .body
            .push_cmt(Mov, vec![target, value], comment);
    }

    fn compile_bin(&mut self, tgt: Name, op: BinOp, left: Value, right: Value, comment: String) {
        let target = self.lookup(tgt);
        let op = translate_binop(op);
        let left = self.get_operand(left);
        let right = self.get_operand(right);

        self.procedure
            .body
            .push_cmt(Mov, vec![target, left], format!("<store> {}", comment));
        self.procedure
            .body
            .push_cmt(op, vec![target, right], format!("<apply> {}", comment));
    }

    fn compile_call(&mut self, tgt: Name, name: Builtin, param_count: usize) {
        let tgt = self.lookup(tgt);

        if param_count > 4 {
            todo!("Can't deal with more than 4 parameters yet.");
        }
        let target_regs = [Rcx, Rdx, R8, R9];
        for (idx, &reg) in target_regs[0..param_count].iter().enumerate() {
            let value = self.param_stack.pop().expect("Parameter count mismatch!");
            self.procedure.body.push_cmt(
                Mov,
                vec![Reg(reg), value],
                format!("set parameter #{}", idx + 1),
            );
        }

        let name = match name {
            Builtin::Print => "print",
        };

        self.procedure
            .body
            .push_cmt(Call, vec![Id(name)], "call print")
            .push_cmt(Mov, vec![tgt, Reg(Rax)], "store return value");
    }

    fn compile_param(&mut self, param: Value) {
        let param = self.get_operand(param);

        self.param_stack.push(param);
    }

    fn get_operand(&mut self, value: Value) -> Operand {
        match value {
            Value::Const(lit) => Lit(lit as i128),
            Value::Name(name) => self.lookup(name),
        }
    }

    fn lookup(&mut self, name: Name) -> Operand {
        let r = self
            .allocator
            .lookup(&name)
            .or_else(|| self.allocator.allocate(name))
            .or_else(|| {
                self.spills.push(name);
                self.procedure.body.push_cmt(Push, vec![], "")
            })

        todo!();
    }

    /// Frees a register. If it was already allocated to a variable, its value is moved around.
    fn free(&mut self, reg: Register) {
        match self.allocator.holds(&reg).cloned() {
            None => (), // The register is already free
            Some(name) => {
                self.allocator.release(&name);
                self.procedure
                    .body
                    .push_cmt(Push, vec![Reg(reg)], format!("free {}", reg));
                self.spills.push(name);
            }
        }
    }
}

fn translate_binop(op: BinOp) -> Op {
    match op {
        BinOp::Add => Add,
        BinOp::Multiply => Imul,
        BinOp::Subtract => Sub,
        _ => todo!("Don't know how to translate {:?} to assembly", op),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! sub {
        ($name:expr) => {
            Name::Sub($name.to_string(), 1)
        };
    }

    macro_rules! cnst {
        ($value:expr) => {
            Value::Const($value)
        };
    }

    macro_rules! listing {
        ($instr:expr) => {{
            let mut listing = TacListing::new();
            listing.push(Instruction::new($instr));
            listing
        }};
    }

    macro_rules! compile_listing {
        ($instr:expr) => {{
            let listing = listing!($instr);
            let proc = Procedure::new("main", Block::new(), Block::new());
            let proc = ProcedureCompiler::compile(listing, proc);
            println!(
                "Listing source:\n===============\n{}===============\n",
                proc
            );
            proc
        }};
    }

    #[test]
    fn compile_tac_compiles_instruction_to_assembly() {
        let procedure =
            compile_listing!(InstrKind::Bin(sub!("x"), BinOp::Add, cnst!(10), cnst!(99),));

        let mut expected = Block::new();
        expected.push(Mov, vec![Reg(Rax), Lit(10)]);
        expected.push(Add, vec![Reg(Rax), Lit(99)]);

        assert_eq!(expected, procedure.body)
    }

    #[test]
    fn can_compile_multiplication() {
        compile_listing!(InstrKind::Bin(
            sub!("x"),
            BinOp::Multiply,
            cnst!(10),
            cnst!(3),
        ));
    }
}
