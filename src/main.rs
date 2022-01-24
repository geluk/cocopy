//! # CocoPy
//!
//! A ChocoPy compiler.

#![feature(lint_reasons)]

use std::{env, fs, iter};

use anyhow::Result;

use ast::untyped::Program;
use error::{CompileError, CompileErrors};

mod ast;
mod builtins;
mod codegen;
mod error;
mod ext;
mod il;
mod lexer;
mod parser;
mod pretty_print;
mod source_map;
mod span;
mod type_checking;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    let content = fs::read_to_string(&args[1])?;

    match run_frontend(&content) {
        Ok(program) => run_backend(program)?,
        Err(errors) => {
            for error in errors.iter() {
                describe_error(error, &content);
            }
        }
    }

    Ok(())
}

pub fn run_frontend(source: &str) -> Result<Program, CompileErrors> {
    let tokens = lexer::lex(source)?;
    println!("\n==============");
    println!("Lexer finished");
    println!("==============\n");
    for token in &tokens {
        println!("{:?}", token);
        println!("{:#?}", token.source.lookup(source));
    }

    let program = parser::parse(&tokens)?;
    println!("\n===============");
    println!("Parser finished");
    println!("===============\n");
    println!("{:#?}\n", program);
    println!("{}", program);

    type_checking::verify_well_typed(&program)?;
    println!("\n=====================");
    println!("Type checker finished");
    println!("=====================\n");

    Ok(program)
}

pub fn run_backend(program: Program) -> Result<()> {
    let il = il::generate(program);

    println!("\n======================");
    println!("IL generation finished");
    println!("======================\n");
    for instr in &il {
        println!("{}", instr);
    }

    let il = il::optimise(il);

    println!("\n======================");
    println!("IL optimisation finished");
    println!("======================\n");
    for instr in &il {
        println!("{}", instr);
    }

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
