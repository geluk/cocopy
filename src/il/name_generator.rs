use std::collections::HashMap;

use crate::ext::{hash_map::ConstHashMap, ordered_hash_map::OrderedHashMap};

use super::{Name, TempVariable, Variable};

pub struct NameGenerator {
    temp_index: usize,
    seen_subscripts: OrderedHashMap<String, usize>,
}
impl NameGenerator {
    pub fn new() -> Self {
        Self {
            temp_index: 0,
            seen_subscripts: OrderedHashMap::new(),
        }
    }

    /// Generates a new unique temporary name.
    pub fn next_temp(&mut self) -> Name {
        self.temp_index += 1;
        Name::Temp(TempVariable::new(self.temp_index))
    }

    /// Generates a unique subscripted name from an existing identifier.
    pub fn next_subscript<S: Into<String>>(&mut self, id: S) -> Variable {
        let id = id.into();
        let value = self
            .seen_subscripts
            .insert_or_modify(id.clone(), || 1, |cur| *cur += 1);

        Variable::new(id, *value)
    }

    /// Returns the most recently generated subscripted name for an identifier.
    pub fn last_subscript<S: Into<String>>(&self, id: S) -> Variable {
        let id = id.into();

        let current_subscript = self
            .seen_subscripts
            .get(&id)
            .copied()
            .expect("Unknown variable");
        Variable::new(id, current_subscript)
    }

    pub fn get_live_variables(&self) -> impl Iterator<Item = Variable> + '_ {
        self.seen_subscripts
            .iter()
            .map(|(n, &s)| Variable::new(n.clone(), s))
    }

    pub fn register_generation(&mut self, variable: &Variable) {
        self.seen_subscripts
            .insert(variable.name.clone(), variable.subscript);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_temp_generates_ascending_temp_values() {
        let mut name_gen = NameGenerator::new();

        assert_eq!("%1", name_gen.next_temp().to_string());
        assert_eq!("%2", name_gen.next_temp().to_string());
    }

    #[test]
    fn next_subscript_generates_ascending_subscripts() {
        let mut name_gen = NameGenerator::new();

        assert_eq!("var^1", name_gen.next_subscript("var").to_string());
        assert_eq!("var^2", name_gen.next_subscript("var").to_string());
    }

    #[test]
    fn last_subscript_produces_most_recent_subscript() {
        let mut name_gen = NameGenerator::new();

        assert_eq!("var^1", name_gen.next_subscript("var").to_string());
        assert_eq!("var^1", name_gen.last_subscript("var").to_string());
        assert_eq!("var^2", name_gen.next_subscript("var").to_string());
        assert_eq!("var^2", name_gen.last_subscript("var").to_string());
    }

    #[test]
    fn variables_can_be_mixed_freely() {
        let mut name_gen = NameGenerator::new();

        assert_eq!("a^1", name_gen.next_subscript("a").to_string());
        assert_eq!("b^1", name_gen.next_subscript("b").to_string());
        assert_eq!("a^2", name_gen.next_subscript("a").to_string());
        assert_eq!("c^1", name_gen.next_subscript("c").to_string());
        assert_eq!("b^1", name_gen.last_subscript("b").to_string());
    }
}
