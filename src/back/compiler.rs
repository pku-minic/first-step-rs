use crate::collection;
use crate::define::{AstBox, AstVisitor, NestedMap, Operator};
use crate::define::{FunDefRc, FunctionDef, Inst, ValRc, Value};
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

/// Compiler.
pub struct Compiler {
  gen: Generator,
}

/// `Result` for `Compiler`.
pub type Result = std::result::Result<Option<ValRc>, &'static str>;

impl Compiler {
  /// Creates a new `Compiler`.
  pub fn new() -> Self {
    Self {
      gen: Generator {
        func: None,
        funcs: collection! {
          "input".to_string() => Rc::new(RefCell::new(FunctionDef::new_lib("input".to_string(), 0))),
          "print".to_string() => Rc::new(RefCell::new(FunctionDef::new_lib("print".to_string(), 1))),
        },
        vars: NestedMap::new(),
        label_id: 0,
      },
    }
  }

  /// Compiles the specific AST.
  pub fn compile(&mut self, ast: AstBox) -> Result {
    self.gen.visit(&ast)
  }

  /// Dumps RISC-V assembly of all compiled ASTs.
  pub fn dump(&self, writer: &mut impl io::Write) -> io::Result<()> {
    for (_, func) in self.gen.funcs.iter() {
      func.borrow().dump(writer)?;
    }
    Ok(())
  }
}

/// IR generator.
struct Generator {
  /// Current function.
  func: Option<FunDefRc>,
  /// All defined functions.
  funcs: HashMap<String, FunDefRc>,
  /// All defined variables.
  vars: NestedMap<String, ValRc>,
  /// Current label id.
  label_id: usize,
}

impl Generator {
  fn new_label(&mut self) -> ValRc {
    let label = Rc::new(Value::Label { id: self.label_id });
    self.label_id += 1;
    label
  }

  fn func(&self) -> RefMut<'_, FunctionDef> {
    self.func.as_ref().unwrap().borrow_mut()
  }
}

impl AstVisitor for Generator {
  type Result = Result;

  fn visit_fundef(&mut self, name: &String, args: &[String], body: &AstBox) -> Self::Result {
    // check argument count
    (args.len() <= 8)
      .then(|| ())
      .ok_or("argument count must be less than or equal to 8")?;
    // create function definition IR
    let func = Rc::new(RefCell::new(FunctionDef::new(name.clone(), args.len())));
    self.func = Some(func.clone());
    // add to function map
    self
      .funcs
      .insert(name.clone(), func)
      .map_or(Ok(()), |_| Err("function has already been defined"))?;
    // enter argument environment
    self.vars.push();
    // add definitions of arguments
    for (i, arg) in args.iter().enumerate() {
      self.vars.add(arg.clone(), Rc::new(Value::ArgRef { id: i }));
    }
    // generate body
    self.visit(body)?;
    // exit argument environment
    self.vars.pop();
    Ok(None)
  }

  fn visit_block(&mut self, stmts: &[AstBox]) -> Self::Result {
    // enter a new environment
    self.vars.push();
    // generate on all statements
    for stmt in stmts {
      self.visit(stmt)?;
    }
    // exit the current environment
    self.vars.pop();
    Ok(None)
  }

  fn visit_define(&mut self, name: &String, expr: &AstBox) -> Self::Result {
    // generate expression
    let expr = self.visit(expr)?.unwrap();
    // add symbol definition
    let slot = self.func().add_slot();
    self
      .vars
      .add(name.clone(), slot.clone())
      .then(|| ())
      .ok_or("symbol has already been defined")?;
    // generate assign instruction
    self.func().push_inst(Box::new(Inst::Assign {
      dest: slot,
      val: expr,
    }));
    Ok(None)
  }

  fn visit_assign(&mut self, name: &String, expr: &AstBox) -> Self::Result {
    // generate expression
    let expr = self.visit(expr)?.unwrap();
    // get stack slot of the symbol
    let slot = self
      .vars
      .get_rec(name)
      .ok_or("symbol has not been defined")?;
    // generate assign instruction
    self.func().push_inst(Box::new(Inst::Assign {
      dest: slot.clone(),
      val: expr,
    }));
    Ok(None)
  }

  fn visit_if(&mut self, cond: &AstBox, then: &AstBox, else_then: &Option<AstBox>) -> Self::Result {
    // generate condition
    let cond = self.visit(cond)?.unwrap();
    // create labels
    let false_branch = self.new_label();
    let end_if = else_then.is_some().then(|| self.new_label());
    // generate conditional branch
    let branch = Box::new(Inst::BranchEqz {
      cond: cond,
      label: false_branch.clone(),
    });
    self.func().push_inst(branch);
    // generate the true branch
    self.visit(then)?;
    else_then.is_some().then(|| {
      self.func().push_inst(Box::new(Inst::Jump {
        label: end_if.clone().unwrap(),
      }))
    });
    // generate the false branch
    self.func().push_inst(Box::new(Inst::Label {
      label: false_branch,
    }));
    else_then.as_ref().map_or(Ok(None), |ast| {
      self.visit(ast)?;
      self.func().push_inst(Box::new(Inst::Label {
        label: end_if.unwrap(),
      }));
      Ok(None)
    })
  }

  fn visit_return(&mut self, expr: &AstBox) -> Self::Result {
    // generate return value
    let expr = self.visit(expr)?.unwrap();
    // generate return instruction
    self.func().push_inst(Box::new(Inst::Return { val: expr }));
    Ok(None)
  }

  fn visit_binary(&mut self, op: &Operator, lhs: &AstBox, rhs: &AstBox) -> Self::Result {
    // check if is logical operator
    if *op == Operator::LAnd || *op == Operator::LOr {
      // logical AND operation, generate labels
      let end_logic = self.new_label();
      // generate lhs first
      let lhs = self.visit(lhs)?.unwrap();
      // generate conditional branch
      self.func().push_inst(Box::new(if *op == Operator::LAnd {
        Inst::BranchEqz {
          cond: lhs.clone(),
          label: end_logic.clone(),
        }
      } else {
        Inst::BranchNez {
          cond: lhs.clone(),
          label: end_logic.clone(),
        }
      }));
      // generate rhs
      let rhs = self.visit(rhs)?.unwrap();
      self.func().push_inst(Box::new(Inst::Assign {
        dest: lhs.clone(),
        val: rhs,
      }));
      // generate label definition
      self
        .func()
        .push_inst(Box::new(Inst::Label { label: end_logic }));
      Ok(Some(lhs))
    } else {
      // generate lhs & rhs
      let lhs = self.visit(lhs)?.unwrap();
      let rhs = self.visit(rhs)?.unwrap();
      // generate binary operation
      let dest = self.func().add_slot();
      self.func().push_inst(Box::new(Inst::Binary {
        dest: dest.clone(),
        op: op.clone(),
        lhs: lhs,
        rhs: rhs,
      }));
      Ok(Some(dest))
    }
  }

  fn visit_unary(&mut self, op: &Operator, opr: &AstBox) -> Self::Result {
    // generate operand
    let opr = self.visit(opr)?.unwrap();
    // generate unary operation
    let dest = self.func().add_slot();
    self.func().push_inst(Box::new(Inst::Unary {
      dest: dest.clone(),
      op: op.clone(),
      opr: opr,
    }));
    Ok(Some(dest))
  }

  fn visit_funcall(&mut self, name: &String, args: &[AstBox]) -> Self::Result {
    // get the function definition
    let func = self.funcs.get(name).ok_or("function not found")?.clone();
    // check argument count
    (args.len() == func.borrow().arg_num())
      .then(|| ())
      .ok_or("argument count mismatch")?;
    // generate arguments
    let args: std::result::Result<Vec<_>, _> = args
      .iter()
      .map(|ast| self.visit(ast).map(|v| v.unwrap()))
      .collect();
    // generate function call
    let dest = self.func().add_slot();
    self.func().push_inst(Box::new(Inst::Call {
      dest: dest.clone(),
      func: Rc::downgrade(&func),
      args: args?,
    }));
    Ok(Some(dest))
  }

  fn visit_int(&mut self, val: &i32) -> Self::Result {
    Ok(Some(Rc::new(Value::Integer { val: *val })))
  }

  fn visit_id(&mut self, val: &String) -> Self::Result {
    // get stack slot of the symbol
    Ok(Some(
      self
        .vars
        .get_rec(val)
        .ok_or("symbol has not been defined")?
        .clone(),
    ))
  }
}
