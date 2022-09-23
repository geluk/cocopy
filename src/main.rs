//! # CocoPy
//!
//! A ChocoPy compiler.

#![feature(lint_reasons)]
#![cfg_attr(test, feature(assert_matches))]
#![deny(non_snake_case)]

use std::{
    fs, iter,
    path::PathBuf,
    process::{self, Command},
};

use ast::typed::Program;
use clap::Parser;
use commandline::{Args, Operation};
use error::{CompileError, CompileErrors};
use prelude::*;

mod ast;
mod builtins;
mod codegen;
mod commandline;
mod error;
mod ext;
mod il;
mod lexer;
mod parser;
mod prelude;
mod pretty_print;
mod source_map;
mod span;
mod type_checking;

fn main() -> Result<()> {
    stderrlog::new().verbosity(1).init()?;

    let args = Args::parse();

    match args.operation {
        Operation::Check { file } => {
            let content = fs::read_to_string(file)?;
            match run_frontend(&content) {
                Ok(_) => {
                    info!("Finished!");
                }
                Err(errors) => {
                    describe_errors(errors, &content);
                    process::exit(1);
                }
            }
        }
        Operation::Compile { file } => {
            let content = fs::read_to_string(file)?;
            match run_frontend(&content) {
                Ok(program) => {
                    run_backend(program)?;
                    info!("Finished!");
                }
                Err(errors) => {
                    describe_errors(errors, &content);
                    process::exit(1);
                }
            }
        }
        Operation::Run { file } => {
            let content = fs::read_to_string(file)?;
            match run_frontend(&content) {
                Ok(program) => {
                    let executable = run_backend(program)?;
                    info!("Compiler finished, starting program.");
                    Command::new(executable).spawn()?;
                }
                Err(errors) => {
                    describe_errors(errors, &content);
                    process::exit(1);
                }
            }
        }
    };
    Ok(())
}

/// Run the compiler frontend. The frontend converts an input string to a
/// [`Program`], which represents an abstract syntax tree.
pub fn run_frontend(source: &str) -> Result<Program, CompileErrors> {
    let tokens = lexer::lex(source)?;
    info!("Lexer finished");
    for token in &tokens {
        let src_str = format!("{:#?}", token.source.lookup(source));
        debug!("{:<12} = {:?} ", src_str, token);
    }

    let untyped_prog = parser::parse(&tokens)?;
    info!("Parser finished");
    debug!("AST:\n{:#?}\n", untyped_prog);
    debug!("Pretty-printed:\n{}\n", untyped_prog);

    let typed_prog = type_checking::verify_well_typed(untyped_prog)?;
    info!("Type checker finished");

    Ok(typed_prog)
}

/// Run the compiler backend. The backend takes an AST and lowers it into a
/// simple intermediate language, which then gets converted to assembly.
/// Finally, the assembly source code is assembled and linked into
/// an executable.
pub fn run_backend(program: Program) -> Result<PathBuf> {
    let il = il::generate(program);

    info!("IL generation finished");
    debug!("IL source:\n{}", il);

    let il = il::optimise(il);

    info!("IL optimisation finished");
    debug!("Optimised IL source:\n{}", il);

    info!("Generating native code");
    codegen::generate_native(il, "out")
}

/// Describe all compile errors.
fn describe_errors(errors: CompileErrors, content: &str) {
    println!();
    println!("{} error(s):", errors.len());
    for error in errors.iter() {
        describe_error(error, content);
    }
}

/// Describe a single compile error. Provides an indication of where the error
/// occurred and what it was caused by.
pub fn describe_error(err: &CompileError, content: &str) {
    let context = source_map::find_line(content, err.range().start());
    let padding = err.range().start() - context.range().start();

    fn pad_char(ch: char, times: usize) -> String {
        iter::repeat(ch).take(times).collect()
    }

    let gutter = format!("{}", context.line_no());

    println!("{} |", pad_char(' ', gutter.len()));
    println!("{} | {}", gutter, context.for_display());
    println!(
        "{} | {}{}--- {}\n",
        pad_char(' ', gutter.len()),
        pad_char(' ', padding.into()),
        pad_char('^', err.length().into()),
        err.describe()
    );
}
