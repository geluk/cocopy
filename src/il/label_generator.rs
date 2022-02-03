use std::collections::HashMap;

use super::Label;

pub struct LabelGenerator {
    seen_subscripts: HashMap<String, usize>,
}
impl LabelGenerator {
    pub fn new() -> Self {
        Self {
            seen_subscripts: HashMap::new(),
        }
    }

    /// Generates a new unique label.
    pub fn next_label<S: Into<String>>(&mut self, id: S) -> Label {
        let id = id.into();
        let current_subscript = self.seen_subscripts.entry(id.clone()).or_insert(0);
        *current_subscript += 1;

        Label::new(id, *current_subscript)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_subscript_generates_ascending_subscripts() {
        let mut lbl_gen = LabelGenerator::new();

        assert_eq!("lbl_1", lbl_gen.next_label("lbl").to_string());
        assert_eq!("lbl_2", lbl_gen.next_label("lbl").to_string());
    }

    #[test]
    fn labels_can_be_mixed_freely() {
        let mut name_gen = LabelGenerator::new();

        assert_eq!("a_1", name_gen.next_label("a").to_string());
        assert_eq!("b_1", name_gen.next_label("b").to_string());
        assert_eq!("a_2", name_gen.next_label("a").to_string());
        assert_eq!("c_1", name_gen.next_label("c").to_string());
        assert_eq!("b_2", name_gen.next_label("b").to_string());
    }
}
