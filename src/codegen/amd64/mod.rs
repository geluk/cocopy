//! Native code generation for 64-bit x86 platforms.

mod assembly;
mod calling_convention;
pub mod linux;
mod procedure_compiler;
mod register_allocator;
mod stack_convention;
pub mod windows;
mod x86;
