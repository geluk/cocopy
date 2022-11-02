//! Assignment of variables to registers.

mod allocation;
mod allocator;

pub use allocation::{BaseOffset, Destination, Lifetime, Move, OneWayMove, Swap};
pub use allocator::Allocator;
