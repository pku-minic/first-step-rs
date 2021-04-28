use super::Lexer;
use crate::define;
use crate::ok_or_return;
use define::{Ast, AstBox, Keyword, Operator, Token};
use std::io::Read;

/// Parser for `first-step` language.
pub struct Parser<T: Read> {
  lexer: Lexer<T>,
  cur_token: super::lexer::Result,
}

/// Error information of `Parser`.
#[derive(Debug)]
pub enum Error {
  /// End of parsing process
  End,
  /// Parser error
  Error(String),
}

/// `Result` for parser functions of `Parser`
pub type Result = std::result::Result<AstBox, Error>;

impl<T: Read> Parser<T> {
  /// Creates a new `Parser` object from the specific `Lexer`.
  pub fn new(lexer: Lexer<T>) -> Self {
    let mut parser = Parser {
      lexer: lexer,
      cur_token: Ok(Token::End),
    };
    parser.next_token();
    parser
  }

  /// Parses the next AST.
  pub fn parse_next(&mut self) -> Result {
    match &self.cur_token {
      Ok(Token::End) => Err(Error::End),
      Ok(_) => self.parse_fundef(),
      Err(err) => Err(Error::Error(err.to_string())),
    }
  }

  /// Gets the next token and returns it.
  fn next_token(&mut self) -> &super::lexer::Result {
    self.cur_token = self.lexer.next_token();
    &self.cur_token
  }

  /// Parses function definitions.
  fn parse_fundef(&mut self) -> Result {
    // get function name
    let name = ok_or_return!(self.expect_id());
    // check & eat '('
    if let Some(err) = self.expect_char('(') {
      return Err(err);
    }
    // get formal arguments
    let mut args = Vec::new();
    if !self.is_token_char(')') {
      loop {
        // get name of the current argument
        args.push(ok_or_return!(self.expect_id()));
        // eat ','
        if !self.is_token_char(',') {
          break;
        }
        self.next_token();
      }
    }
    // check & eat ')'
    if let Some(err) = self.expect_char(')') {
      return Err(err);
    }
    // get function body
    self.parse_block().map(|body| {
      Box::new(Ast::FunDef {
        name: name,
        args: args,
        body: body,
      })
    })
  }

  /// Parses blocks.
  fn parse_block(&mut self) -> Result {
    // check & eat '{'
    if let Some(err) = self.expect_char('{') {
      return Err(err);
    }
    // get statements
    let mut stmts = Vec::new();
    while !self.is_token_char('}') {
      stmts.push(ok_or_return!(self.parse_statement()));
    }
    // eat '}'
    self.next_token();
    Ok(Box::new(Ast::Block { stmts: stmts }))
  }

  /// Parses statements.
  fn parse_statement(&mut self) -> Result {
    return match &self.cur_token {
      Ok(Token::Id(id)) => {
        let id = id.to_string();
        self.parse_define_assign(id)
      }
      Ok(Token::Key(Keyword::If)) => self.parse_if_else(),
      Ok(Token::Key(Keyword::Return)) => self.parse_return(),
      _ => Self::get_error("invalid statement"),
    };
  }

  /// Parses define/assign statements.
  fn parse_define_assign(&mut self, id: String) -> Result {
    // eat id
    self.next_token();
    // check if is a function call
    if self.is_token_char('(') {
      return self.parse_funcall(id);
    }
    // check if is define/assign
    let is_define = self.is_token_op(Operator::Define);
    if !is_define && !self.is_token_op(Operator::Assign) {
      return Self::get_error("expected ':=' or '='");
    }
    self.next_token();
    // get expression
    self.parse_expr().map(|expr| {
      if is_define {
        Box::new(Ast::Define {
          name: id,
          expr: expr,
        })
      } else {
        Box::new(Ast::Assign {
          name: id,
          expr: expr,
        })
      }
    })
  }

  /// Parses if-else statements.
  fn parse_if_else(&mut self) -> Result {
    // eat 'if'
    self.next_token();
    // get condition
    let cond = ok_or_return!(self.parse_expr());
    // get 'then' body
    let then = ok_or_return!(self.parse_block());
    // check & get 'else-then' body
    Ok(Box::new(Ast::If {
      cond: cond,
      then: then,
      else_then: if self.is_token_key(Keyword::Else) {
        // eat 'else'
        self.next_token();
        // parse 'if' or block of 'else'
        Some(ok_or_return!(if self.is_token_key(Keyword::If) {
          self.parse_if_else()
        } else {
          self.parse_block()
        }))
      } else {
        None
      },
    }))
  }

  /// Parses return statements.
  fn parse_return(&mut self) -> Result {
    // eat 'return'
    self.next_token();
    // get return value
    self
      .parse_expr()
      .map(|expr| Box::new(Ast::Return { expr: expr }))
  }

  /// Parses expressions.
  fn parse_expr(&mut self) -> Result {
    let f = |p: &mut Parser<T>| p.parse_land_expr();
    self.parse_binary(f, &[Operator::LOr])
  }

  /// Parses logical AND expressions.
  fn parse_land_expr(&mut self) -> Result {
    let f = |p: &mut Parser<T>| p.parse_eq_expr();
    self.parse_binary(f, &[Operator::LAnd])
  }

  /// Parses EQ expressions.
  fn parse_eq_expr(&mut self) -> Result {
    let f = |p: &mut Parser<T>| p.parse_rel_expr();
    self.parse_binary(f, &[Operator::Eq, Operator::NotEq])
  }

  /// Parses relation expressions.
  fn parse_rel_expr(&mut self) -> Result {
    let f = |p: &mut Parser<T>| p.parse_add_expr();
    self.parse_binary(f, &[Operator::Less, Operator::LessEq])
  }

  /// Parses add/sub expressions.
  fn parse_add_expr(&mut self) -> Result {
    let f = |p: &mut Parser<T>| p.parse_mul_expr();
    self.parse_binary(f, &[Operator::Add, Operator::Sub])
  }

  /// Parses mul/div/mod expressions.
  fn parse_mul_expr(&mut self) -> Result {
    let f = |p: &mut Parser<T>| p.parse_unary();
    self.parse_binary(f, &[Operator::Mul, Operator::Div, Operator::Mod])
  }

  /// Parses unary expressions.
  fn parse_unary(&mut self) -> Result {
    // check if is unary expression
    if let Ok(Token::Op(op)) = &self.cur_token {
      let op = op.clone();
      self.next_token();
      // check if is a valid unary operator
      match op {
        Operator::Sub | Operator::LNot => (),
        _ => return Self::get_error("invalid unary operator"),
      }
      // get operand
      self
        .parse_expr()
        .map(|expr| Box::new(Ast::Unary { op: op, opr: expr }))
    } else {
      self.parse_value()
    }
  }

  /// Parses values.
  fn parse_value(&mut self) -> Result {
    match &self.cur_token {
      Ok(Token::Int(int)) => {
        // get integer value
        let val = *int;
        self.next_token();
        // integer literal
        Ok(Box::new(Ast::Int { val: val }))
      }
      Ok(Token::Id(id)) => {
        // eat id
        let id = id.to_string();
        self.next_token();
        // check if is a function call
        if self.is_token_char('(') {
          self.parse_funcall(id)
        } else {
          Ok(Box::new(Ast::Id { id: id }))
        }
      }
      Ok(Token::Other(c)) if *c == '(' => {
        // eat '('
        self.next_token();
        // get expression
        let expr = ok_or_return!(self.parse_expr());
        // check & eat ')'
        if let Some(err) = self.expect_char(')') {
          return Err(err);
        }
        Ok(expr)
      }
      _ => Self::get_error("invalid value"),
    }
  }

  /// Parses function calls.
  fn parse_funcall(&mut self, id: String) -> Result {
    // eat '('
    self.next_token();
    // get arguments
    let mut args = Vec::new();
    if !self.is_token_char(')') {
      loop {
        // get the current argument
        args.push(ok_or_return!(self.parse_expr()));
        // eat ','
        if !self.is_token_char(',') {
          break;
        }
        self.next_token();
      }
    }
    // check & eat ')'
    if let Some(err) = self.expect_char(')') {
      return Err(err);
    }
    Ok(Box::new(Ast::FunCall {
      name: id.to_string(),
      args: args,
    }))
  }

  /// Parses binary expression.
  fn parse_binary<F>(&mut self, parser: F, ops: &[Operator]) -> Result
  where
    F: Fn(&mut Parser<T>) -> Result,
  {
    // get left-hand side expression
    let mut lhs = ok_or_return!(parser(self));
    // get the rest things
    loop {
      // stop if error
      let op = match self.is_token_ops(ops) {
        Some(op) => op,
        None => break,
      };
      self.next_token();
      // get right-hand side expression
      let rhs = ok_or_return!(parser(self));
      // update lhs
      lhs = Box::new(Ast::Binary {
        op: op,
        lhs: lhs,
        rhs: rhs,
      })
    }
    Ok(lhs)
  }

  /// Returns a parser error.
  fn get_error(message: &'static str) -> Result {
    Err(Error::Error(String::from(message)))
  }

  /// Expects an identifier from lexer.
  fn expect_id(&mut self) -> std::result::Result<String, Error> {
    if let Ok(Token::Id(id)) = &self.cur_token {
      let id = id.to_string();
      self.next_token();
      Ok(id)
    } else {
      Err(Error::Error(String::from("expected identifier")))
    }
  }

  /// Expects the specific character from lexer.
  fn expect_char(&mut self, c: char) -> Option<Error> {
    if !self.is_token_char(c) {
      Some(Error::Error(format!("expected '{}'", c)))
    } else {
      None
    }
  }

  /// Checks if the current token is the specific character.
  fn is_token_char(&self, c: char) -> bool {
    self
      .cur_token
      .as_ref()
      .map_or(false, |t| *t == Token::Other(c))
  }

  /// Checks if the current token is the specific operator.
  fn is_token_op(&self, op: Operator) -> bool {
    self
      .cur_token
      .as_ref()
      .map_or(false, |t| *t == Token::Op(op))
  }

  /// Checks if the current token is one of the specific operators.
  /// Returns the operator if matched.
  fn is_token_ops(&self, ops: &[Operator]) -> Option<Operator> {
    match &self.cur_token {
      Ok(Token::Op(op)) if ops.iter().find(|&x| *op == *x).is_some() => Some(op.clone()),
      _ => None,
    }
  }

  /// Checks if the current token is the specific keyword.
  fn is_token_key(&self, key: Keyword) -> bool {
    self
      .cur_token
      .as_ref()
      .map_or(false, |t| *t == Token::Key(key))
  }
}
