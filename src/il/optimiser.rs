use std::collections::HashMap;

use super::{Instruction, TacListing, Value};

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
            .listing
            .iter_lines()
            .filter_map(|(line, instr)| {
                if let Instruction::Assign(target, _) = instr {
                    if !self.listing.is_used_after(target, line) {
                        return Some(line);
                    }
                }
                None
            })
            .collect();

        self.remove_lines(candidates);
    }

    /// Remove single assignments, and replace all occurrences
    /// of those names with their constant.
    fn merge_assign(&mut self) {
        let mut replacements = HashMap::new();

        let const_assignments = self
            .listing
            .iter_lines()
            .filter_map(|(line, instr)| match instr {
                Instruction::Assign(name, value) => {
                    replacements.insert(name.clone(), value.clone());
                    Some(line)
                }
                _ => None,
            })
            .collect();

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

        self.remove_lines(const_assignments);
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
