use super::Lexer;
use crate::define;
use define::{AstBox, Keyword, Operator, Token};
use std::io::Read;

/// Parser for `first-step` language.
pub struct Parser<T: Read> {
  lexer: Lexer<T>,
  cur_token: super::lexer::Result,
}

/// Error information of `Parser`.
pub enum Error {
  /// End of parsing process
  End,
  /// Parser error
  Error(&'static str),
}

/// `Result` for parser functions of `Parser`
type Result = std::result::Result<AstBox, Error>;

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
    match self.cur_token {
      Ok(Token::End) => Err(Error::End),
      Ok(_) => self.parse_fundef(),
      Err(err) => Err(Error::Error(err)),
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
    let name = match self.expect_id() {
      Ok(id) => id,
      Err(err) => return Err(err),
    };
    // check & eat '('
    if let Some(err) = self.expect_char('(') {
      return Err(err);
    }
    // get formal arguments
    let args = Vec::new();
    if !self.is_token_char(')') {
      loop {
        // get name of the current argument
        args.push(match self.expect_id() {
          Ok(id) => id,
          Err(err) => return Err(err),
        });
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
      Box::new(define::FunDefAst {
        name: name,
        args: args,
        body: body,
      }) as AstBox
    })
  }

  /// Parses blocks.
  fn parse_block(&mut self) -> Result {
    // check & eat '{'
    if let Some(err) = self.expect_char('{') {
      return Err(err);
    }
    // get statements
    let stmts = Vec::new();
    while !self.is_token_char('}') {
      stmts.push(match self.parse_statement() {
        Ok(stmt) => stmt,
        err => return err,
      })
    }
    // eat '}'
    self.next_token();
    Ok(Box::new(define::BlockAst { stmts: stmts }))
  }

  /// Parses statements.
  fn parse_statement(&mut self) -> Result {
    if let Ok(token) = self.cur_token {
      return match token {
        Token::Id(id) => self.parse_define_assign(id),
        Token::Key(Keyword::If) => self.parse_if_else(),
        Token::Key(Keyword::Return) => self.parse_return(),
      };
    }
    Err(Error::Error("invalid statement"))
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
      return Err(Error::Error("expected ':=' or '='"));
    }
    self.next_token();
    // get expression
    self.parse_expr().map(|expr| {
      if is_define {
        Box::new(define::DefineAst {
          name: id,
          expr: expr,
        }) as AstBox
      } else {
        Box::new(define::AssignAst {
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
    let cond = match self.parse_expr() {
      Ok(ast) => ast,
      err => return err,
    };
    // get 'then' body
    let then = match self.parse_block() {
      Ok(ast) => ast,
      err => return err,
    };
    // check & get 'else-then' body
    Ok(Box::new(define::IfAst {
      cond: cond,
      then: then,
      else_then: if self.is_token_key(Keyword::Else) {
        // eat 'else'
        self.next_token();
        // parse 'if' or block of 'else'
        match if self.is_token_key(Keyword::If) {
          self.parse_if_else()
        } else {
          self.parse_block()
        } {
          Ok(ast) => Some(ast),
          err => return err,
        }
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
      .map(|expr| Box::new(define::ReturnAst { expr: expr }) as AstBox)
  }

  /// Parses expressions.
  fn parse_expr(&mut self) -> Result {
    self.parse_binary(|| self.parse_land_expr(), &[Operator::LOr])
  }

  /// Parses logical AND expressions.
  fn parse_land_expr(&mut self) -> Result {
    self.parse_binary(|| self.parse_eq_expr(), &[Operator::LAnd])
  }

  /// Parses EQ expressions.
  fn parse_eq_expr(&mut self) -> Result {
    self.parse_binary(|| self.parse_rel_expr(), &[Operator::Eq, Operator::NotEq])
  }

  /// Parses relation expressions.
  fn parse_rel_expr(&mut self) -> Result {
    self.parse_binary(
      || self.parse_add_expr(),
      &[Operator::Less, Operator::LessEq],
    )
  }

  /// Parses add/sub expressions.
  fn parse_add_expr(&mut self) -> Result {
    self.parse_binary(|| self.parse_mul_expr(), &[Operator::Add, Operator::Sub])
  }

  /// Parses mul/div/mod expressions.
  fn parse_mul_expr(&mut self) -> Result {
    self.parse_binary(
      || self.parse_unary(),
      &[Operator::Mul, Operator::Div, Operator::Mod],
    )
  }

  /// Parses unary expressions.
  fn parse_unary(&mut self) -> Result {
    // check if is unary expression
    if let Ok(Token::Op(op)) = self.cur_token {
      self.next_token();
      // check if is a valid unary operator
      match op {
        Operator::Sub | Operator::LNot => (),
        _ => return Err(Error::Error("invalid unary operator")),
      }
      // get operand
      self
        .parse_expr()
        .map(|expr| Box::new(define::UnaryAst { op: op, opr: expr }) as AstBox)
    } else {
      self.parse_value()
    }
  }

  /// Parses values.
  fn parse_value(&mut self) -> Result {
    match self.cur_token {
      Ok(Token::Int(int)) => {
        self.next_token();
        // integer literal
        Ok(Box::new(define::IntAst { val: int }))
      }
      Ok(Token::Id(id)) => {
        // eat identifier
        self.next_token();
        // check if is a function call
        if self.is_token_char('(') {
          self.parse_funcall(id)
        } else {
          Ok(Box::new(define::IdAst { id: id }))
        }
      }
      Ok(Token::Other(c)) if c == '(' => {
        // eat '('
        self.next_token();
        // get expression
        let expr = match self.parse_expr() {
          Ok(ast) => ast,
          err => return err,
        };
        // check & eat ')'
        if let Some(err) = self.expect_char(')') {
          return Err(err);
        }
        Ok(expr)
      }
      _ => Err(Error::Error("invalid value")),
    }
  }

  /// Parses function calls.
  fn parse_funcall(&mut self, id: String) -> Result {
    // eat '('
    self.next_token();
    // get arguments
    let args = Vec::new();
    if !self.is_token_char(')') {
      loop {
        // get the current argument
        args.push(match self.parse_expr() {
          Ok(ast) => ast,
          err => return err,
        });
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
    Ok(Box::new(define::FunCallAst {
      name: id,
      args: args,
    }))
  }

  /// Parses binary expression.
  fn parse_binary<F>(&mut self, parser: F, ops: &[Operator]) -> Result
  where
    F: FnMut() -> Result,
  {
    // get left-hand side expression
    let mut lhs = match parser() {
      Ok(ast) => ast,
      err => return err,
    };
    // get the rest things
    loop {
      // stop if error
      let op = match self.cur_token {
        Ok(Token::Op(op)) if ops.iter().find(|&&x| op == x).is_some() => op,
        _ => break,
      };
      self.next_token();
      // get right-hand side expression
      let rhs = match parser() {
        Ok(ast) => ast,
        err => return err,
      };
      // update lhs
      lhs = Box::new(define::BinaryAst {
        op: op,
        lhs: lhs,
        rhs: rhs,
      })
    }
    Ok(lhs)
  }

  /// Expects an identifier from lexer.
  fn expect_id(&mut self) -> std::result::Result<String, Error> {
    let cur_token = self.cur_token;
    self.next_token();
    if let Ok(Token::Id(id)) = cur_token {
      Ok(id)
    } else {
      Err(Error::Error("expected identifier"))
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
    self.cur_token.map_or(false, |t| t == Token::Other(c))
  }

  /// Checks if the current token is the specific operator.
  fn is_token_op(&self, op: Operator) -> bool {
    self.cur_token.map_or(false, |t| t == Token::Op(op))
  }

  /// Checks if the current token is the specific keyword.
  fn is_token_key(&self, key: Keyword) -> bool {
    self.cur_token.map_or(false, |t| t == Token::Key(key))
  }
}
