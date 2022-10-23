//! Native code generation for 64-bit x86 platforms.

mod assembly;
mod calling_convention;
mod defer;
mod lifetime_analysis;
pub mod linux;
mod op_semantics;
mod procedure_compiler;
mod stack_convention;
pub mod windows;
mod x86;
