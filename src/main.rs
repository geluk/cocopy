use std::{env, fs, iter};

use anyhow::Result;
use lexer::LexError;

mod codegen;
mod lexer;
mod parser;
mod pretty_print;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    let content = fs::read_to_string(&args[1])?;

    let result = lexer::lex(&content);

    println!("\n==============");
    println!("Lexer finished");
    println!("==============\n");

    match result {
        Ok(tokens) => {
            let parse_result = parser::parse(&tokens);

            for token in tokens.iter() {
                println!("{:#?}", token);
                println!("{:#?}", &content[token.source.clone()]);
            }

            println!("\n===============");
            println!("Parser finished");
            println!("===============\n");

            match parse_result {
                Ok(expr) => println!("{:#?}", expr),
                Err(err) => println!("Parse failure! {:#?}", err),
            }
        }
        Err(errors) => {
            for err in errors {
                describe_error(&err, &content);
            }
        }
    }

    Ok(())
}

fn describe_error(err: &LexError, content: &str) {
    let (line_no, line_start, error_line) = find_line(content, err.range.start);
    let padding = err.range.start - line_start;

    fn pad_char(ch: char, times: usize) -> String {
        iter::repeat(ch).take(times).collect()
    }

    let gutter = format!("{}", line_no);

    println!("{}| {}", gutter, error_line);
    println!(
        "{}| {}{}--- {}",
        pad_char(' ', gutter.len()),
        pad_char(' ', padding),
        pad_char('^', err.length()),
        err.error_type
    );

    println!("Lexer error ({})", err.error_type);
}

fn find_line(source: &str, target_position: usize) -> (usize, usize, &str) {
    let lines = source.split_inclusive(|c| c == '\n' || c == '\r');
    let mut position = 0usize;
    for (line_idx, line) in lines.enumerate() {
        let end_position = position + line.len();
        if target_position >= position && target_position <= end_position {
            return (line_idx + 1, position, line.trim_end());
        }
        position = end_position;
    }

    panic!("Target position was outside the bounds of the source string")
}
