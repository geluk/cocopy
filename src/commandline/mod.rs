use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[clap(name = "cocopy")]
#[clap(about = "A compiler for ChocoPy")]
pub struct Args {
    #[clap(subcommand)]
    pub operation: Operation,
}

#[derive(Debug, Subcommand)]
pub enum Operation {
    /// Check a program for errors
    Check { file: String },
    /// Compile a program
    Compile { file: String },
    /// Compile and run
    Run { file: String },
}
