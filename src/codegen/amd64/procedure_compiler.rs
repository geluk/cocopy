use crate::{
    ast::untyped::BinOp,
    codegen::amd64::x86::{Op, Register},
    il::*,
};

use super::{assembly::*, register_allocator::RegisterAllocator};

use Op::*;
use Operand::*;

pub struct ProcedureCompiler {
    allocator: RegisterAllocator,
    procedure: Procedure,
}

impl ProcedureCompiler {
    pub fn compile(listing: TacListing, procedure: Procedure) -> (Procedure, Option<Register>) {
        let mut compiler = Self {
            allocator: RegisterAllocator::new(),
            procedure,
        };
        let oper = compiler.compile_tac(listing);

        (compiler.procedure, oper)
    }

    pub fn compile_tac(&mut self, listing: TacListing) -> Option<Register> {
        let mut final_tgt = None;
        for (_, instr) in listing.iter_lines() {
            let oper = self.compile_instr(instr);
            // TODO: After each instruction has been compiled, iterate over allocated registers,
            // and free those whose value will no longer be used.
            final_tgt.replace(oper);
        }
        final_tgt
    }

    fn compile_instr(&mut self, instr: &Instruction) -> Register {
        let comment = instr.to_string();
        match instr {
            Instruction::Assign(tgt, value) => {
                self.compile_assign(tgt.clone(), value.clone(), comment)
            }
            Instruction::Bin(tgt, op, left, right) => {
                self.compile_bin(tgt.clone(), *op, left.clone(), right.clone(), comment)
            }
        }
    }

    fn compile_assign(&mut self, target: Name, value: Value, comment: String) -> Register {
        let target = self.allocator.lookup(target);
        let value = self.get_operand(value);

        self.procedure
            .body
            .push_cmt(Mov, vec![Reg(target), value], comment);

        target
    }

    fn compile_bin(
        &mut self,
        tgt: Name,
        op: BinOp,
        left: Value,
        right: Value,
        comment: String,
    ) -> Register {
        let target = self.allocator.lookup(tgt);
        let op = translate_binop(op);
        let left = self.get_operand(left);
        let right = self.get_operand(right);

        self.procedure
            .body
            .push_cmt(Mov, vec![Reg(target), left], format!("<store> {}", comment));
        self.procedure
            .body
            .push_cmt(op, vec![Reg(target), right], format!("<apply> {}", comment));
        target
    }

    fn get_operand(&mut self, value: Value) -> Operand {
        match value {
            Value::Const(lit) => Lit(lit as i128),
            Value::Name(name) => Reg(self.allocator.lookup(name)),
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
    use Register::*;

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
            listing.push($instr);
            listing
        }};
    }

    macro_rules! compile_listing {
        ($instr:expr) => {{
            let listing = listing!($instr);
            let proc = Procedure::new("main", Block::new(), Block::new());
            let (proc, _) = ProcedureCompiler::compile(listing, proc);
            println!(
                "Listing source:\n===============\n{}===============\n",
                proc
            );
            proc
        }};
    }

    #[test]
    fn compile_tac_compiles_instruction_to_assembly() {
        let procedure = compile_listing!(Instruction::Bin(
            sub!("x"),
            BinOp::Add,
            cnst!(10),
            cnst!(99),
        ));

        let mut expected = Block::new();
        expected.push(Mov, vec![Reg(RAX), Lit(10)]);
        expected.push(Add, vec![Reg(RAX), Lit(99)]);

        assert_eq!(expected, procedure.body)
    }

    #[test]
    fn can_compile_multiplication() {
        compile_listing!(Instruction::Bin(
            sub!("x"),
            BinOp::Multiply,
            cnst!(10),
            cnst!(3),
        ));
    }
}
