//! Intermediate code generation.
//!
//! In cocopy, intermediate code is represented as three-address code (TAC).
//! As the name implies, each TAC instruction will take at most three operands,
//! though some will take less. Often, instructions will take the form of
//! `x = y <> z`, where `<>` represents some binary operation.
//! Other instructions may take the form of `x = y` or `x = ~ y`, where `~` is
//! a unary operator.
//!
//! Instructions are guaranteed to be in static single-assignment form (SSA).

mod generator;
mod label_generator;
mod name_generator;
mod optimiser;
mod phi;
mod tac;

pub use generator::generate;
pub use optimiser::optimise;
pub use tac::*;
