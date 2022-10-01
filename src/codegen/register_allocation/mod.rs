//! Assignment of variables to registers.

mod allocation;
mod allocator;

pub use allocation::{BaseOffset, Destination, Lifetime};
pub use allocator::Allocator;
