use clap::{Args, Parser, Subcommand};

#[derive(Debug, Parser)]
#[clap(about = "A compiler for ChocoPy")]
pub struct Options {
    #[clap(subcommand)]
    pub operation: Operation,
    #[clap(short, long, default_value_t = 1)]
    pub verbose: usize,
}

#[derive(Debug, Subcommand)]
pub enum Operation {
    /// Check a program for errors
    Check { file: String },
    /// Compile a program
    Compile {
        file: String,
        #[clap(flatten)]
        backend: BackendOptions,
    },
    /// Compile and run
    Run {
        file: String,
        #[clap(flatten)]
        backend: BackendOptions,
    },
    /// Reassemble the previously generated program
    Reassemble,
}

#[derive(Debug, Args)]
pub struct BackendOptions {
    #[clap(short, long)]
    /// Do not optimise the generated code
    no_optimise: bool,
}

impl BackendOptions {
    pub fn optimise(&self) -> bool {
        !self.no_optimise
    }
}
