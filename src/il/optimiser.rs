use std::{collections::HashMap, iter::Enumerate, slice::Iter};

use super::{Instruction, MatchInstruction, TacListing, Value};

pub fn optimise(listing: TacListing) -> TacListing {
    let mut optimiser = Optimiser::new(listing);
    optimiser.optimise();
    optimiser.listing
}

struct Optimiser {
    listing: TacListing,
}
impl Optimiser {
    pub fn new(listing: TacListing) -> Self {
        Self { listing }
    }

    pub fn optimise(&mut self) {
        self.remove_unused_assignments();
        self.merge_assign();
    }

    /// Remove assignments to names that are never read.
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
            .iter_lines()
            .match_instruction(Instruction::as_assign)
            .filter(|(line, (name, _))| !self.listing.is_used_after(name, *line))
            .map(|(line, _)| line)
            .collect();

        self.remove_lines(candidates);
    }

    /// Looks for single assignments (`x` = `y`), replaces all occurrences
    /// of `x` with `y` and deletes the original assignment instruction.
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

        let mut assignments = vec![];
        for (line, (name, value)) in self.iter_lines().match_instruction(Instruction::as_assign) {
            replacements.insert(name.clone(), value.clone());
            assignments.push(line);
        }

        for instr in self.listing.iter_mut() {
            if let Some(name) = replacements
                .keys()
                .filter(|name| instr.reads_from_name(name))
                .next()
            {
                let replacement = replacements.get(name).cloned().unwrap();
                instr.replace(&Value::Name(name.clone()), replacement);
            }
        }

        self.remove_lines(assignments);
    }

    fn iter_lines(&self) -> Enumerate<Iter<Instruction>> {
        self.listing.iter_lines()
    }

    fn remove_lines(&mut self, mut lines: Vec<usize>) {
        lines.sort_unstable();
        lines.reverse();
        for line in lines.into_iter() {
            self.listing.remove(line);
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
                .into_vec()
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
            vec!["%t1 = 10 + 10"]
        )
    }
}
