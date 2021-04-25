use crate::define::Operator;

/// Interfaces of ASTs.
pub trait AST {
  /// Evaluates AST using the specific interpreter.
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result
  where
    Self: Sized;

  /// Generates IR using the specific IR generator.
  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result
  where
    Self: Sized;
}

/// Box for ASTs.
pub type ASTBox = Box<dyn AST>;

/// Interfaces of AST visitors.
pub trait ASTVisitor {
  /// Result type of visitor methods.
  type Result;

  /// Visits function definitions.
  fn visit_fundef(&mut self, ast: &FunDefAST) -> Self::Result;
  /// Visits block statements.
  fn visit_block(&mut self, ast: &BlockAST) -> Self::Result;
  /// Visits define statements.
  fn visit_define(&mut self, ast: &DefineAST) -> Self::Result;
  /// Visits assign statements.
  fn visit_assign(&mut self, ast: &AssignAST) -> Self::Result;
  /// Visits if-else statements.
  fn visit_if(&mut self, ast: &IfAST) -> Self::Result;
  /// Visits binary expressions.
  fn visit_binary(&mut self, ast: &BinaryAST) -> Self::Result;
  /// Visits unary expressions.
  fn visit_unary(&mut self, ast: &UnaryAST) -> Self::Result;
  /// Visits function calls.
  fn visit_funcall(&mut self, ast: &FunCallAST) -> Self::Result;
  /// Visits integer literals.
  fn visit_int(&mut self, ast: &IntAST) -> Self::Result;
  /// Visits identifiers.
  fn visit_id(&mut self, ast: &IdAST) -> Self::Result;
}

/// Function definition.
pub struct FunDefAST {
  pub name: String,
  pub args: Vec<String>,
  pub body: ASTBox,
}

impl AST for FunDefAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_fundef(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_fundef(self)
  }
}

/// Statement block.
pub struct BlockAST {
  pub stmts: Vec<ASTBox>,
}

impl AST for BlockAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_block(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_block(self)
  }
}

/// Define statement.
pub struct DefineAST {
  pub name: String,
  pub expr: ASTBox,
}

impl AST for DefineAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_define(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_define(self)
  }
}

/// Assign statement.
pub struct AssignAST {
  pub name: String,
  pub expr: ASTBox,
}

impl AST for AssignAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_assign(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_assign(self)
  }
}

/// If-else statement.
pub struct IfAST {
  pub cond: ASTBox,
  pub then: ASTBox,
  pub else_then: ASTBox,
}

impl AST for IfAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_if(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_if(self)
  }
}

/// Binary expression.
pub struct BinaryAST {
  pub op: Operator,
  pub lhs: ASTBox,
  pub rhs: ASTBox,
}

impl AST for BinaryAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_binary(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_binary(self)
  }
}

/// Unary expression.
pub struct UnaryAST {
  pub op: Operator,
  pub opr: ASTBox,
}

impl AST for UnaryAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_unary(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_unary(self)
  }
}

/// Function call.
pub struct FunCallAST {
  pub name: String,
  pub args: Vec<ASTBox>,
}

impl AST for FunCallAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_funcall(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_funcall(self)
  }
}

/// Integer literal.
pub struct IntAST {
  pub val: i32,
}

impl AST for IntAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_int(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_int(self)
  }
}

/// Identifier.
pub struct IdAST {
  pub id: String,
}

impl AST for IdAST {
  fn eval<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_id(self)
  }

  fn generate_ir<T: ASTVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_id(self)
  }
}
