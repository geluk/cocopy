//! Assignment of variables to registers.

mod allocation;
mod allocator;

pub use allocation::{Allocation, BaseOffset, Lifetime};
pub use allocator::{Allocator, Destination};
