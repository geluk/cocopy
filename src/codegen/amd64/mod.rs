//! Native code generation for 64-bit x86 platforms

mod assembly;
pub mod linux;
mod procedure_compiler;
mod register_allocator;
pub mod windows;
mod x86;
