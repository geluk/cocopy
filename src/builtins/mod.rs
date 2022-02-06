use std::collections::HashMap;

use crate::ast::TypeSpec;

pub fn get_type_map() -> HashMap<String, (Builtin, TypeSpec)> {
    let mut type_map = HashMap::new();

    type_map.insert(
        "print".to_string(),
        (
            Builtin::Print,
            TypeSpec::Function(vec![TypeSpec::Int], Box::new(TypeSpec::None)),
        ),
    );

    type_map
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Builtin {
    Print,
}
