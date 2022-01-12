use crate::parser::syntax_tree::BinExpr;

pub fn compile(expression: BinExpr) -> Program {
    let program = Program::new();

    program
}

#[derive(Default)]
pub struct Program {
    text: Section,
    data: Section,
}

impl Program {
    pub fn new() -> Self {
        let mut program = Self {
            text: Section::new(".text".to_string()),
            data: Section::new(".data".to_string()),
        };
        program
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
