use crate::parser::syntax_tree::BinExpr;

pub fn compile(_expression: BinExpr) -> Program {
    Program::new()
}

#[derive(Default)]
pub struct Program {
    text: Section,
    data: Section,
}

impl Program {
    pub fn new() -> Self {
        Self {
            text: Section::new(".text".to_string()),
            data: Section::new(".data".to_string()),
        }
    }
}

#[derive(Default)]
pub struct Section {
    name: String,
    lines: Vec<String>,
}

impl Section {
    pub fn new(name: String) -> Self {
        Self {
            name,
            lines: vec![],
        }
    }
    pub fn push(&mut self, value: String) {
        self.lines.push(value)
    }
}
