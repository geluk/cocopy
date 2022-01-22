use super::Name;

pub struct NameGenerator {
    index: usize,
}

impl NameGenerator {
    pub fn new() -> Self {
        Self { index: 0 }
    }

    pub fn next(&mut self) -> Name {
        self.index += 1;
        Name::Temp(format!("t{}", self.index))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_generator_generates_ascending_unique_values() {
        let mut name_gen = NameGenerator { index: 0 };

        assert_eq!("%t1", name_gen.next().to_string());
        assert_eq!("%t2", name_gen.next().to_string());
    }
}
