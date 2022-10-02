use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use crate::listing::Position;

use super::{MatchInstruction, TacInstr, TacListing, TacProgram, Value};

pub fn optimise(mut program: TacProgram) -> TacProgram {
    program.top_level = Optimiser::optimise(program.top_level);

    program.functions = program
        .functions
        .into_iter()
        .map(|(name, func)| (name, Optimiser::optimise(func)))
        .collect();

    program
}

struct Optimiser {
    listing: TacListing,
}
impl Optimiser {
    pub fn optimise(listing: TacListing) -> TacListing {
        let mut optimiser = Self { listing };

        optimiser.remove_unused_assignments();
        optimiser.remove_unused_return_assignments();
        optimiser.merge_assign();
        optimiser.remove_phi();

        optimiser.listing
    }

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
            .listing
            .iter_lines()
            .match_instruction(TacInstr::as_assign)
            .filter(|(line, (name, _))| !self.listing.is_used_after(name, *line))
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
            .listing
            .iter_lines()
            .match_instruction(TacInstr::as_call)
            .filter_map(|(line, (name, _, _))| name.map(|n| (line, n)))
            .filter(|(line, name)| !self.listing.is_used_after(name, *line))
            .map(|(line, _)| line)
            .collect();

        for (_, instr) in self
            .listing
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

        for (line, (name, value)) in self
            .listing
            .iter_lines()
            .match_instruction(TacInstr::as_assign)
        {
            replacements.insert(name.clone(), (line, value.clone()));
        }

        let mut assignments = vec![];
        for (name, (line, value)) in replacements {
            if self
                .listing
                .iter_instructions_mut()
                .all(|instr| instr.may_replace(&name))
            {
                for instr in self.listing.iter_instructions_mut() {
                    instr.replace(&name, Cow::Borrowed(&value));
                }
                assignments.push(line);
            }
        }

        self.remove_lines(assignments);
    }

    /// Deletes all ɸ-functions, replacing all assignments to the variables it receives as arguments
    /// with the variable that the ɸ-function assigns to. This is essentially the reverse of
    /// [`Self::merge_assign`].
    /// This optimiser step should be the last one to run, since after this step, the code will no
    /// longer be in static single-assignment form.
    ///
    /// Optimises:
    /// ```
    /// b^1 = 100
    /// if_false a^1 goto if_end
    /// b^2 = 2
    /// if_end:
    /// b^3 = ɸ(b^1, b^2)
    /// arg b^3
    /// ```
    /// To:
    /// ```
    /// b^3 = 100
    /// if_false a^1 goto if_end
    /// b^3 = 2
    /// if_end:
    /// arg b^3
    /// ```
    fn remove_phi(&mut self) {
        let mut phi_fns = vec![];
        let replacements: HashMap<_, _> = self
            .listing
            .iter_lines()
            .match_instruction(TacInstr::as_phi)
            .flat_map(|(line, (dest, sources))| {
                phi_fns.push(line);
                sources.iter().map(|src| (src.clone(), dest.clone()))
            })
            .collect();

        self.remove_lines(phi_fns);

        for (src_name, dest_name) in replacements {
            for instr in self.listing.iter_instructions_mut() {
                instr.replace_assign(&src_name, Cow::Borrowed(&dest_name))
            }
            let dest_value = Value::Name(dest_name);
            for instr in self.listing.iter_instructions_mut() {
                instr.replace(&src_name, Cow::Borrowed(&dest_value));
            }
        }
    }

    fn remove_lines(&mut self, mut lines: Vec<Position>) {
        lines.sort_unstable();
        lines.reverse();
        for line in lines.into_iter() {
            self.listing.remove(line);
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
                .into_instructions()
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
