use crate::define::Operator;

/// Interfaces of ASTs.
pub trait Ast {
  /// Evaluates AST using the specific interpreter.
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result
  where
    Self: Sized;

  /// Generates IR using the specific IR generator.
  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result
  where
    Self: Sized;
}

/// Box for ASTs.
pub type AstBox = Box<dyn Ast>;

/// Interfaces of AST visitors.
pub trait AstVisitor {
  /// Result type of visitor methods.
  type Result;

  /// Visits function definitions.
  fn visit_fundef(&mut self, ast: &FunDefAst) -> Self::Result;
  /// Visits block statements.
  fn visit_block(&mut self, ast: &BlockAst) -> Self::Result;
  /// Visits define statements.
  fn visit_define(&mut self, ast: &DefineAst) -> Self::Result;
  /// Visits assign statements.
  fn visit_assign(&mut self, ast: &AssignAst) -> Self::Result;
  /// Visits if-else statements.
  fn visit_if(&mut self, ast: &IfAst) -> Self::Result;
  /// Visits return statements.
  fn visit_return(&mut self, ast: &ReturnAst) -> Self::Result;
  /// Visits binary expressions.
  fn visit_binary(&mut self, ast: &BinaryAst) -> Self::Result;
  /// Visits unary expressions.
  fn visit_unary(&mut self, ast: &UnaryAst) -> Self::Result;
  /// Visits function calls.
  fn visit_funcall(&mut self, ast: &FunCallAst) -> Self::Result;
  /// Visits integer literals.
  fn visit_int(&mut self, ast: &IntAst) -> Self::Result;
  /// Visits identifiers.
  fn visit_id(&mut self, ast: &IdAst) -> Self::Result;
}

/// Function definition.
pub struct FunDefAst {
  pub name: String,
  pub args: Vec<String>,
  pub body: AstBox,
}

impl Ast for FunDefAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_fundef(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_fundef(self)
  }
}

/// Statement block.
pub struct BlockAst {
  pub stmts: Vec<AstBox>,
}

impl Ast for BlockAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_block(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_block(self)
  }
}

/// Define statement.
pub struct DefineAst {
  pub name: String,
  pub expr: AstBox,
}

impl Ast for DefineAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_define(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_define(self)
  }
}

/// Assign statement.
pub struct AssignAst {
  pub name: String,
  pub expr: AstBox,
}

impl Ast for AssignAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_assign(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_assign(self)
  }
}

/// If-else statement.
pub struct IfAst {
  pub cond: AstBox,
  pub then: AstBox,
  pub else_then: Option<AstBox>,
}

impl Ast for IfAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_if(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_if(self)
  }
}

/// Return statement.
pub struct ReturnAst {
  pub expr: AstBox,
}

impl Ast for ReturnAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_return(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_return(self)
  }
}

/// Binary expression.
pub struct BinaryAst {
  pub op: Operator,
  pub lhs: AstBox,
  pub rhs: AstBox,
}

impl Ast for BinaryAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_binary(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_binary(self)
  }
}

/// Unary expression.
pub struct UnaryAst {
  pub op: Operator,
  pub opr: AstBox,
}

impl Ast for UnaryAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_unary(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_unary(self)
  }
}

/// Function call.
pub struct FunCallAst {
  pub name: String,
  pub args: Vec<AstBox>,
}

impl Ast for FunCallAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_funcall(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_funcall(self)
  }
}

/// Integer literal.
pub struct IntAst {
  pub val: i32,
}

impl Ast for IntAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_int(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_int(self)
  }
}

/// Identifier.
pub struct IdAst {
  pub id: String,
}

impl Ast for IdAst {
  fn eval<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_id(self)
  }

  fn generate_ir<T: AstVisitor>(&self, visitor: &mut T) -> T::Result {
    visitor.visit_id(self)
  }
}
