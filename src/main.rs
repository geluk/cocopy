use std::{env, fs, iter};

use anyhow::Result;
use error::{CompileError, CompileErrors};

mod codegen;
mod error;
mod lexer;
mod parser;
mod pretty_print;
mod source_map;
mod span;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    let content = fs::read_to_string(&args[1])?;

    if let Err(errors) = compile(&content) {
        for error in errors.iter() {
            describe_error(error, &content);
        }
    }

    Ok(())
}

pub fn compile(source: &str) -> Result<(), CompileErrors> {
    let tokens = lexer::lex(source)?;
    for token in &tokens {
        println!("{:#?}", token);
        println!("{:#?}", token.source.lookup(source));
    }
    println!("\n==============");
    println!("Lexer finished");
    println!("==============\n");

    let program = parser::parse(&tokens)?;
    println!("\n===============");
    println!("Parser finished");
    println!("===============\n");
    println!("{:#?}\n", program);
    println!("{}", program);

    Ok(())
}

pub fn describe_error(err: &CompileError, content: &str) {
    let context = source_map::find_line(content, err.range().start());
    let err_start = err.range().start();
    let ctx_start = context.range().start();
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
