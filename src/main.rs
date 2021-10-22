//! An interpreter for [Thorsten Ball's Monkey programming language](https://monkeylang.org/)
//! implemented in Rust.

use rustyline::error::ReadlineError;
use rustyline::Editor;
use structopt::StructOpt;

use crate::eval::eval;
use crate::lexer::Lexer;
use crate::parser::Parser;

mod ast;
mod built_in;
mod eval;
mod lexer;
mod object;
mod operator;
mod parser;
mod scope;

#[derive(Debug, PartialEq)]
enum OutputMode {
  Ast,
  Normal,
  Tokens,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "Monkey Interpreter")]
struct Config {
  /// Output Mode - can be `normal` (default) to output the result of the
  /// evaluated input, `ast` to output a formatted abstract syntax tree
  /// generated by the parser, or `tokens` to output the tokens generated by
  /// the lexer.
  #[structopt(short, long)]
  output: Option<String>,
}

/// Starts a Monkey REPL.
fn main() {
  let config = Config::from_args();

  let output_mode = match config.output.as_deref() {
    Some("ast") => OutputMode::Ast,
    Some("tokens") => OutputMode::Tokens,
    _ => OutputMode::Normal,
  };

  let mut editor = Editor::<()>::new();

  println!();
  println!("Monkey REPL");
  println!("press ctrl+c to quit");
  println!();

  loop {
    let readline = editor.readline("> ");

    match readline {
      Ok(line) => {
        println!();
        println!("{}", monkey(&line, &output_mode));
        println!();
      },
      Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
        break;
      },
      Err(err) => {
        println!("Error reading input: {:?}", err);
        break;
      },
    }
  }
}

/// Takes Monkey input and produces the desired output.
fn monkey(input: &str, output_mode: &OutputMode) -> String {
  let tokens = Lexer::new(input).tokens();

  if output_mode == &OutputMode::Tokens {
    return format!("> Tokens: {:#?}", tokens);
  }

  let program = Parser::new(tokens).parse();

  if !program.errors.is_empty() {
    return format!("Encountered errors during parsing: {:#?}", program.errors);
  }

  if output_mode == &OutputMode::Ast {
    return format!("> AST: {:#?}", program.ast);
  }

  match eval(program.ast) {
    Ok(object) => format!("> {}", object),
    Err(eval_err) => format!("Encountered an error during evaluation: {}", eval_err),
  }
}
