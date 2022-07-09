//! # CocoPy
//!
//! A ChocoPy compiler.

#![feature(lint_reasons)]
#![cfg_attr(test, feature(assert_matches))]
#![deny(non_snake_case)]

use std::{env, fs, iter, process};

use ast::typed::Program;
use error::{CompileError, CompileErrors};
use prelude::*;

mod ast;
mod builtins;
mod codegen;
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
    let args: Vec<_> = env::args().collect();

    let content = fs::read_to_string(&args[1])?;

    match run_frontend(&content) {
        Ok(program) => {
            run_backend(program)?;
            println!("finished!");
            println!("======================\n");
            Ok(())
        }
        Err(errors) => {
            for error in errors.iter() {
                describe_error(error, &content);
            }
            process::exit(1);
        }
    }
}

/// Run the compiler frontend. The frontend converts an input string to a
/// [`Program`], which represents an abstract syntax tree.
pub fn run_frontend(source: &str) -> Result<Program, CompileErrors> {
    let tokens = lexer::lex(source)?;
    println!("\n==============");
    println!("Lexer finished");
    println!("==============\n");
    for token in &tokens {
        println!("{:?}", token);
        println!("{:#?}", token.source.lookup(source));
    }

    let untyped_prog = parser::parse(&tokens)?;
    println!("\n===============");
    println!("Parser finished");
    println!("===============\n");
    println!("{:#?}\n", untyped_prog);
    println!("{}", untyped_prog);

    let typed_prog = type_checking::verify_well_typed(untyped_prog)?;
    println!("\n=====================");
    println!("Type checker finished");
    println!("=====================\n");

    Ok(typed_prog)
}

/// Run the compiler backend. The backend takes an AST and lowers it into a
/// simple intermediate language, which then gets converted to assembly.
/// Finally, the assembly source code is assembled and linked into
/// an executable.
pub fn run_backend(program: Program) -> Result<()> {
    let il = il::generate(program);

    println!("\n======================");
    println!("IL generation finished");
    println!("======================\n");
    println!("{}", il);

    let il = il::optimise(il);

    println!("\n========================");
    println!("IL optimisation finished");
    println!("========================\n");
    println!("{}", il);

    println!("======================");
    println!("Generating native code");
    codegen::generate_native(il, "out")
}

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
