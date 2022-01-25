use super::{Instruction, Name, Value};

pub fn optimise(instructions: Vec<Instruction>) -> Vec<Instruction> {
    let mut optimiser = Optimiser::new(instructions);
    optimiser.optimise();
    optimiser.instructions
}

struct Optimiser {
    instructions: Vec<Instruction>,
}
impl Optimiser {
    pub fn new(instructions: Vec<Instruction>) -> Self {
        Self { instructions }
    }

    pub fn optimise(&mut self) {
        let unused_indexes: Vec<_> = self
            .instructions
            .iter()
            .enumerate()
            .filter_map(|(ix, is)| self.filter_unused_assign(ix, is))
            .collect();

        for idx in unused_indexes.into_iter().rev() {
            self.instructions.remove(idx);
        }
    }

    fn filter_unused_assign(&self, index: usize, instr: &Instruction) -> Option<usize> {
        if let Instruction::Assign(target, _) = instr {
            if !self.is_used_after(target, index) {
                return Some(index);
            }
        }
        None
    }

    fn is_used_after(&self, name: &Name, index: usize) -> bool {
        if index >= self.instructions.len() {
            return false;
        }

        self.instructions[index..].iter().any(|instr| match instr {
            Instruction::Assign(_, value) => Self::is_usage_of(name, value),
            Instruction::Bin(_, _, lhs, rhs) => {
                Self::is_usage_of(name, lhs) || Self::is_usage_of(name, rhs)
            }
        })
    }

    fn is_usage_of(name: &Name, value: &Value) -> bool {
        match value {
            Value::Name(n) => name == n,
            Value::Const(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{il::generate, lexer::lex, parser::parse};

    use super::*;

    macro_rules! assert_optimises {
        ($source:expr, $expected:expr) => {{
            let tokens = lex($source).unwrap();
            let ast = parse(&tokens).unwrap();
            let tac = generate(ast);
            let tac = optimise(tac)
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>();

            assert_eq!($expected, tac)
        }};
    }

    #[test]
    fn unused_declaration_is_removed() {
        assert_optimises!("x : int = 10\nx = 20 + 10", vec!["%t1 = 20 + 10"])
    }

    #[test]
    fn unused_declaration_before_intermediate_statement_is_removed() {
        assert_optimises!(
            r#"
x : int = 0
y: int = 10
x = y + y "#,
            vec!["y^1 = 10", "%t1 = y^1 + y^1"]
        )
    }
}
