use crate::define::{Keyword, Operator, Token};
use phf::phf_map;
use std::io::Read;

/// Lexer for `first-step` language.
pub struct Lexer<T: Read> {
  readable: T,
  last_char: Option<char>,
  error_num: usize,
}

/// `Result` for token handlers of `Lexer`.
type Result = std::result::Result<Token, &'static str>;

impl<T: Read> Lexer<T> {
  /// Creates a new `Lexer` object from the specific file.
  pub fn new(readable: T) -> Self {
    Lexer {
      readable: readable,
      last_char: Some(' '),
      error_num: 0,
    }
  }

  /// Gets the next token from file.
  pub fn next_token(&mut self) -> Result {
    // skip spaces
    while self.last_char.map_or(false, |c| c.is_whitespace()) {
      self.next_char();
    }
    // check the last character
    if let Some(c) = self.last_char {
      if c == '#' {
        // skip comments
        self.handle_comment()
      } else if c.is_alphabetic() || c == '_' {
        // id or keyword
        self.handle_id()
      } else if c.is_numeric() {
        // integer literal
        self.handle_integer()
      } else if is_operator_char(c) {
        // operator
        self.handle_operator()
      } else {
        // other characters
        self.next_char();
        Ok(Token::Other(c))
      }
    } else {
      // may be EOF, or other file errors
      Ok(Token::End)
    }
  }

  /// Returns the error number.
  pub fn error_num(&self) -> usize {
    self.error_num
  }

  /// Reads a character from file.
  fn next_char(&mut self) {
    // NOTE: UTF-8 characters will not be handled here.
    let mut single_char = [0];
    if let Ok(_) = self.readable.read(&mut single_char) {
      self.last_char = Some(single_char[0] as char);
    } else {
      self.last_char = None;
    }
  }

  /// Handles identifiers or keywords.
  fn handle_id(&mut self) -> Result {
    // read to string
    let mut id = String::new();
    while self
      .last_char
      .map_or(false, |c| c.is_alphanumeric() || c == '_')
    {
      id.push(self.last_char.unwrap());
      self.next_char();
    }
    // check if string is keyword
    if let Some(keyword) = parse_keyword(&id) {
      Ok(Token::Key(keyword))
    } else {
      Ok(Token::Id(id))
    }
  }

  /// Handles integer literals.
  fn handle_integer(&mut self) -> Result {
    // read to string
    let mut num = String::new();
    while self.last_char.map_or(false, |c| c.is_numeric()) {
      num.push(self.last_char.unwrap());
      self.next_char();
    }
    // convert to integer
    num
      .parse::<i32>()
      .map(|i| Token::Int(i))
      .map_err(|_| "invalid integer literal")
  }

  /// Handles operators.
  fn handle_operator(&mut self) -> Result {
    // read to string
    let mut op = String::new();
    while self.last_char.map_or(false, |c| is_operator_char(c)) {
      op.push(self.last_char.unwrap());
      self.next_char();
    }
    // check if is a valid operator
    parse_operator(&op)
      .map(|op| Token::Op(op))
      .ok_or("invalid operator")
  }

  /// Handles comment.
  fn handle_comment(&mut self) -> Result {
    // skip the current line
    while self.last_char.map_or(false, |c| c == '\r' || c == '\n') {
      self.next_char();
    }
    // return the next token
    self.next_token()
  }
}

/// Checks whether the specific character may appear in the operator.
fn is_operator_char(c: char) -> bool {
  "+-*/%<=!&|:".contains(c)
}

/// Parses keyword from the specific string.
fn parse_keyword(s: &str) -> Option<Keyword> {
  static KEYWORDS: phf::Map<&'static str, Keyword> = phf_map! {
    "if" => Keyword::If,
    "else" => Keyword::Else,
    "return" => Keyword::Return,
  };
  KEYWORDS.get(s).cloned()
}

/// Parses operator from the specific string.
fn parse_operator(s: &str) -> Option<Operator> {
  static OPERATORS: phf::Map<&'static str, Operator> = phf_map! {
    "+" => Operator::Add,
    "-" => Operator::Sub,
    "*" => Operator::Mul,
    "/" => Operator::Div,
    "%" => Operator::Mod,
    "<" => Operator::Less,
    "<=" => Operator::LessEq,
    "==" => Operator::Eq,
    "!=" => Operator::NotEq,
    "&&" => Operator::LAnd,
    "||" => Operator::LOr,
    "!" => Operator::LNot,
    ":=" => Operator::Define,
    "=" => Operator::Assign,
  };
  OPERATORS.get(s).cloned()
}
