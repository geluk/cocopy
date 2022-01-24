//! Intermediate code generation.

mod generator;
mod name_generator;
mod optimiser;
mod tac;

pub use generator::generate;
pub use optimiser::optimise;
pub use tac::*;
