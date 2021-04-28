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
