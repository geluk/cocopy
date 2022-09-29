use std::collections::HashMap;

use super::{phi::Variables, Name, Variable};

pub struct NameGenerator {
    index: usize,
    seen_subscripts: HashMap<String, usize>,
}
impl NameGenerator {
    pub fn new() -> Self {
        Self {
            index: 0,
            seen_subscripts: HashMap::new(),
        }
    }

    /// Generates a new unique temporary name.
    pub fn next_temp(&mut self) -> Name {
        self.index += 1;
        Name::Temp(self.index)
    }

    /// Generates a unique subscripted name from an existing identifier.
    pub fn next_subscript<S: Into<String>>(&mut self, id: S) -> Variable {
        let id = id.into();
        let current_subscript = self.seen_subscripts.entry(id.clone()).or_insert(0);
        *current_subscript += 1;

        Variable::new(id, *current_subscript)
    }

    /// Returns the most recently generated subscripted name for an identifier.
    pub fn last_subscript<S: Into<String>>(&self, id: S) -> Variable {
        let id = id.into();
        let current_subscript = self.seen_subscripts[&id];
        Variable::new(id, current_subscript)
    }

    pub fn get_live_variables(&self) -> Variables {
        Variables::new(self.seen_subscripts.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_temp_generates_ascending_temp_values() {
        let mut name_gen = NameGenerator::new();

        assert_eq!("%t1", name_gen.next_temp().to_string());
        assert_eq!("%t2", name_gen.next_temp().to_string());
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
