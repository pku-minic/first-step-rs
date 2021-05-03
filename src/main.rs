mod back;
mod define;
mod front;

use back::interpreter::Interpreter;
use front::{lexer::Lexer, parser::Parser};
use std::env;
use std::fs;
use std::process;

fn interpret(file: fs::File) -> Result<i32, String> {
  use front::parser::Error;
  let mut parser = Parser::new(Lexer::new(file));
  let mut intp = Interpreter::new();
  // parse the input file
  loop {
    match parser.parse_next() {
      Ok(ast) => intp.add_func_def(ast)?,
      Err(Error::End) => break,
      Err(Error::Error(err)) => return Err(err),
    }
  }
  // evaluate the program
  Ok(intp.eval()?)
}

fn main() -> Result<(), String> {
  // parse command line arguments
  let args: Vec<_> = env::args().collect();
  if args.len() < 2 {
    println!("usage: {} <INPUT> [-c [-o <OUTPUT>]]", args[0]);
    return Err("invalid command line argument".to_string());
  }
  // open file
  let file = fs::File::open(&args[1]).map_err(|err| format!("{}", err))?;
  // run interpreter
  let code = interpret(file)?;
  process::exit(code);
}
