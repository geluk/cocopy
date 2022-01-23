use std::collections::HashMap;

use crate::ast::TypeSpec;

pub fn get_types() -> HashMap<String, TypeSpec> {
    let mut type_map = HashMap::new();

    type_map.insert(
        "print".to_string(),
        TypeSpec::Function(vec![TypeSpec::Int], Box::new(TypeSpec::None)),
    );

    type_map
}
