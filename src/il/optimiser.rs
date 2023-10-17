use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use crate::{
    il::{Label, Name},
    listing::Position,
};

use super::{BasicBlock, MatchInstruction, TacInstr, TacProcedure, TacProgram};

pub fn optimise(mut program: TacProgram) -> TacProgram {
    optimise_procedure(&mut program.top_level);

    for procedure in program.functions.values_mut() {
        optimise_procedure(procedure);
    }

    program
}

fn optimise_procedure(procedure: &mut TacProcedure) {
    verify_call_args(procedure);

    optimise_block(procedure.entry_block_mut());

    for block in procedure.basic_blocks_mut() {
        optimise_block(block);
    }

    verify_call_args(procedure);
}

fn optimise_block(block: &mut BasicBlock) {
    let mut optimiser = Optimiser { block };

    optimiser.remove_unused_assignments();
    optimiser.remove_unused_return_assignments();
    optimiser.merge_assign();
}

fn verify_call_args(procedure: &mut TacProcedure) {
    let verify_call = |name: &Label, args: &Vec<Name>| {
        let target = procedure.basic_block(name);
        let expected = target.parameters().len();
        let actual = args.len();
        assert_eq!(
            actual, expected,
            "Expected {expected} arguments, but found {actual}"
        );
    };

    for block in procedure.basic_blocks() {
        for instr in block.listing().iter_instructions() {
            match instr {
                TacInstr::Goto(name, args) => verify_call(name, args),
                TacInstr::If(_, name_t, name_f, args) => {
                    verify_call(name_t, args);
                    verify_call(name_f, args);
                }
                TacInstr::IfCmp(_, _, _, name_t, name_f, args) => {
                    verify_call(name_t, args);
                    verify_call(name_f, args);
                }
                _ => (),
            };
        }
    }
}

struct Optimiser<'block> {
    block: &'block mut BasicBlock,
}
impl<'block> Optimiser<'block> {
    /// Remove assignments to names that are never read.
    ///
    /// Optimises:
    /// ```
    /// a^1 = 10
    /// a^2 = 20
    /// b^1 = a^2 * 2
    /// ```
    /// To:
    /// ```
    /// a^2 = 20
    /// b^1 = a^2 * 2
    /// ```
    fn remove_unused_assignments(&mut self) {
        let candidates = self
            .iter_instructions(TacInstr::as_assign)
            .filter(|(line, (name, _))| !self.block.listing().is_used_after(name, *line))
            .map(|(line, _)| line)
            .collect();

        self.remove_lines(candidates);
    }

    /// Remove function call assignments to names that are never read.
    ///
    /// Optimises:
    /// ```
    /// a^1 = call print (10)
    /// ```
    /// To:
    /// ```
    /// call print (10)
    /// ```
    fn remove_unused_return_assignments(&mut self) {
        let candidates: HashSet<_> = self
            .iter_instructions(TacInstr::as_call)
            .filter_map(|(line, (name, _, _))| name.map(|n| (line, n)))
            .filter(|(line, name)| !self.block.listing().is_used_after(name, *line))
            .map(|(line, _)| line)
            .collect();

        for (_, instr) in self
            .block
            .listing_mut()
            .iter_lines_mut()
            .filter(|(l, _)| candidates.contains(l))
        {
            if let TacInstr::Call(tgt, _, _) = instr {
                *tgt = None;
            }
        }
    }

    /// Looks for single assignments (`x` = `y`), replaces all occurrences
    /// of `x` with `y` and deletes the original assignment instruction.
    ///
    /// Optimises:
    /// ```
    /// a^1 = 10
    /// b^1 = a^1 * 2
    /// %t1 = b^1
    /// %t2 = %t1 + a^1
    /// ```
    /// To:
    /// ```
    /// b^1 = 10 * 2
    /// %t2 = b^1 + 10
    /// ```
    fn merge_assign(&mut self) {
        let mut replacements = HashMap::new();

        for (line, (name, value)) in self.iter_instructions(TacInstr::as_assign) {
            replacements.insert(name.clone(), (line, value.clone()));
        }

        let mut assignments = vec![];
        for (name, (line, value)) in replacements {
            if self
                .block
                .listing_mut()
                .iter_instructions_mut()
                .all(|instr| instr.may_replace(&name))
            {
                for instr in self.block.listing_mut().iter_instructions_mut() {
                    instr.replace(&name, Cow::Borrowed(&value));
                }
                assignments.push(line);
            }
        }

        self.remove_lines(assignments);
    }

    fn iter_instructions<'s, P, I>(
        &'s self,
        predicate: P,
    ) -> impl Iterator<Item = (Position, I)> + 's
    where
        P: Fn(&'s TacInstr) -> Option<I> + 's,
    {
        self.block
            .listing()
            .iter_lines()
            .match_instruction(predicate)
    }

    fn remove_lines(&mut self, mut lines: Vec<Position>) {
        lines.sort_unstable();
        lines.reverse();
        for line in lines.into_iter() {
            self.block.listing_mut().remove(line);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{il::generate, lexer::lex, parser::parse, type_checking::verify_well_typed};

    use super::*;

    macro_rules! assert_optimises {
        ($source:expr, $expected:expr) => {{
            let tokens = lex($source).unwrap();
            let ast = parse(&tokens).unwrap();
            let ast = verify_well_typed(ast).unwrap();
            let tac = generate(ast);
            println!("Before opt:\n==========");
            print!("{}", tac.top_level);
            println!("==========");
            let tac = optimise(tac);
            println!("After opt:\n==========");
            print!("{}", tac.top_level);
            println!("==========");
            let tac = tac
                .top_level
                .entry_block()
                .listing()
                .iter_instructions()
                .map(|i| i.to_string())
                .collect::<Vec<_>>();

            assert_eq!($expected, tac)
        }};
    }

    #[test]
    fn unused_declaration_is_removed() {
        assert_optimises!("x : int = 10\nx = 20 + 10", vec!["%1 = 20 + 10"])
    }

    #[test]
    fn unused_declaration_before_intermediate_statement_is_removed() {
        assert_optimises!(
            r#"
x : int = 0
y: int = 10
x = y + y "#,
            vec!["%1 = 10 + 10"]
        )
    }

    #[test]
    fn merge_assign_can_be_applied_multiple_times_in_one_instruction() {
        assert_optimises!(
            r#"
a : int = 1
b : int = 2
c : int = 0
c = a + b"#,
            vec!["%1 = 1 + 2"]
        )
    }
}
