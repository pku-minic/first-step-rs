use crate::define::Operator;

/// ASTs of `first-step`.
pub enum Ast {
  /// Function definition.
  FunDef {
    name: String,
    args: Vec<String>,
    body: AstBox,
  },

  /// Statement block.
  Block { stmts: Vec<AstBox> },

  /// Define statement.
  Define { name: String, expr: AstBox },

  /// Assign statement.
  Assign { name: String, expr: AstBox },

  /// If-else statement.
  If {
    cond: AstBox,
    then: AstBox,
    else_then: Option<AstBox>,
  },

  /// Return statement.
  Return { expr: AstBox },

  /// Binary expression.
  Binary {
    op: Operator,
    lhs: AstBox,
    rhs: AstBox,
  },

  /// Unary expression.
  Unary { op: Operator, opr: AstBox },

  /// Function call.
  FunCall { name: String, args: Vec<AstBox> },

  /// Integer literal.
  Int { val: i32 },

  /// Identifier.
  Id { id: String },
}

/// Box for ASTs.
pub type AstBox = Box<Ast>;

/// AST visitor for visiting ASTs.
pub trait AstVisitor {
  type Result;

  /// Visits an AST.
  fn visit(&mut self, ast: &AstBox) -> Self::Result {
    use Ast::*;
    match ast.as_ref() {
      FunDef { name, args, body } => self.visit_fundef(name, args, body),
      Block { stmts } => self.visit_block(stmts),
      Define { name, expr } => self.visit_define(name, expr),
      Assign { name, expr } => self.visit_assign(name, expr),
      If {
        cond,
        then,
        else_then,
      } => self.visit_if(cond, then, else_then),
      Return { expr } => self.visit_return(expr),
      Binary { op, lhs, rhs } => self.visit_binary(op, lhs, rhs),
      Unary { op, opr } => self.visit_unary(op, opr),
      FunCall { name, args } => self.visit_funcall(name, args),
      Int { val } => self.visit_int(val),
      Id { id } => self.visit_id(id),
    }
  }

  /// Visits function definitions.
  fn visit_fundef(&mut self, name: &String, args: &[String], body: &AstBox) -> Self::Result;
  /// Visits statement blocks.
  fn visit_block(&mut self, stmts: &[AstBox]) -> Self::Result;
  /// Visits define statements.
  fn visit_define(&mut self, name: &String, expr: &AstBox) -> Self::Result;
  /// Visits assign statements.
  fn visit_assign(&mut self, name: &String, expr: &AstBox) -> Self::Result;
  /// Visits if-else statements.
  fn visit_if(&mut self, cond: &AstBox, then: &AstBox, else_then: &Option<AstBox>) -> Self::Result;
  /// Visits return statements.
  fn visit_return(&mut self, expr: &AstBox) -> Self::Result;
  /// Visits binary statements.
  fn visit_binary(&mut self, op: &Operator, lhs: &AstBox, rhs: &AstBox) -> Self::Result;
  /// Visits unary statements.
  fn visit_unary(&mut self, op: &Operator, opr: &AstBox) -> Self::Result;
  /// Visits function calls.
  fn visit_funcall(&mut self, name: &String, args: &[AstBox]) -> Self::Result;
  /// Visits integer literals.
  fn visit_int(&mut self, val: &i32) -> Self::Result;
  /// Visits identifiers.
  fn visit_id(&mut self, val: &String) -> Self::Result;
}
