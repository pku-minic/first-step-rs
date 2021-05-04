mod back;
mod define;
mod front;

use back::{compiler::Compiler, interpreter::Interpreter};
use define::AstBox;
use front::{lexer::Lexer, parser::Parser};
use std::env;
use std::fs;
use std::io;
use std::process;

/// Runs parser.
fn parse<'a, F, T>(file: fs::File, mut action: F) -> Result<(), String>
where
  F: FnMut(AstBox) -> Result<T, String>,
{
  use front::parser::Error;
  let mut parser = Parser::new(Lexer::new(file));
  // parse the input file
  loop {
    match parser.parse_next() {
      Ok(ast) => action(ast)?,
      Err(Error::End) => break,
      Err(Error::Error(err)) => return Err(err),
    };
  }
  Ok(())
}

/// Runs parser & interpreter.
fn interpret(file: fs::File) -> Result<i32, String> {
  // parse the program
  let mut intp = Interpreter::new();
  parse(file, |ast| Ok(intp.add_func_def(ast)?))?;
  // evaluate the program
  Ok(intp.eval()?)
}

/// Runs parser & compiler.
fn compile(file: fs::File, writer: &mut impl io::Write) -> Result<(), String> {
  // parse the program
  let mut comp = Compiler::new();
  parse(file, |ast| Ok(comp.compile(ast)?))?;
  // dump the compiled assembly
  comp.dump(writer).map_err(|err| format!("{}", err))?;
  Ok(())
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
  // check if need to compile the input file
  if args.len() >= 3 && args[2] == "-c" {
    // initialize output stream
    if args.len() >= 5 && args[3] == "-o" {
      let mut writer = fs::File::create(&args[4]).map_err(|err| format!("{}", err))?;
      compile(file, &mut writer)
    } else {
      compile(file, &mut io::stdout())
    }
  } else {
    // run interpreter
    let code = interpret(file)?;
    process::exit(code);
  }
}
