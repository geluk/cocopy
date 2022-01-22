//! Intermediate code generation.

mod generator;
mod name_generator;
mod tac;

pub use generator::generate;
pub use tac::*;
