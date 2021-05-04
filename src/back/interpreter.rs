use crate::define::{Ast, AstBox, AstVisitor};
use crate::define::{NestedMap, Operator};
use crate::unwrap_struct;
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Interpreter for `first-step` language.
pub struct Interpreter {
  /// Implementation of the evaluator.
  intp: InterpreterImpl,
}

/// `Result` for `Interpreter`.
pub type Result = std::result::Result<i32, &'static str>;

impl Interpreter {
  /// Creates a new interpreter.
  pub fn new() -> Self {
    Self {
      intp: InterpreterImpl {
        funcs: Rc::new(RefCell::new(HashMap::new())),
        envs: Rc::new(RefCell::new(NestedMap::new())),
      },
    }
  }

  /// Adds the specific function definition to interpreter
  pub fn add_func_def(&mut self, func: AstBox) -> std::result::Result<(), &'static str> {
    match func.as_ref() {
      // get function name
      Ast::FunDef { name, .. } => {
        // check if is already defined
        if !self.intp.funcs.borrow().contains_key(name) {
          // add function definition
          self.intp.funcs.borrow_mut().insert(name.clone(), func);
          Ok(())
        } else {
          Err("function has already been defined")
        }
      }
      _ => panic!("not a function"),
    }
  }

  /// Evaluates the current program.
  pub fn eval(&mut self) -> Result {
    // find & evaluate the `main` function
    match self.intp.funcs.clone().borrow().get("main") {
      Some(main) => self.intp.visit(main),
      _ => Err("'main' function not found"),
    }
  }
}

/// Implementation of the interpreter.
struct InterpreterImpl {
  /// All function definitions.
  funcs: Rc<RefCell<HashMap<String, AstBox>>>,
  /// Environments.
  envs: Rc<RefCell<NestedMap<String, i32>>>,
}

lazy_static! {
  /// Name of return value when evaluating.
  static ref RET_VAL: String = "$ret".to_string();
}

impl InterpreterImpl {
  /// Performs library function call.
  fn call_lib_func(
    &mut self,
    name: &str,
    args: &[AstBox],
  ) -> std::result::Result<Option<i32>, &'static str> {
    match name {
      "input" => {
        // check arguments
        if !args.is_empty() {
          Err("argument count mismatch")
        } else {
          // read an integer from stdin
          let mut line = String::new();
          std::io::stdin()
            .read_line(&mut line)
            .expect("failed to read from stdin");
          match line.trim().parse::<i32>() {
            Ok(ret) => Ok(Some(ret)),
            _ => Err("invalid input, expected integer"),
          }
        }
      }
      "print" => {
        // check arguments
        if args.len() != 1 {
          Err("argument count mismatch")
        } else {
          // evaluate argument
          let arg = self.visit(args.first().unwrap())?;
          // print to stdout
          println!("{}", arg);
          Ok(Some(0))
        }
      }
      // not a library function call
      _ => Ok(None),
    }
  }
}

impl AstVisitor for InterpreterImpl {
  type Result = Result;

  fn visit_fundef(&mut self, _name: &String, _args: &[String], body: &AstBox) -> Self::Result {
    // set up the default return value
    let ret = self.envs.borrow_mut().add(RET_VAL.clone(), 0);
    debug_assert!(ret, "environment corrupted");
    // evaluate function body
    self.visit(body)?;
    // get return value
    Ok(*self.envs.borrow().get(&RET_VAL, false).unwrap())
  }

  fn visit_block(&mut self, stmts: &[AstBox]) -> Self::Result {
    // enter a new environment
    self.envs.borrow_mut().push();
    // evaluate all statements
    for stmt in stmts {
      self.visit(stmt)?;
    }
    // exit the current environment
    self.envs.borrow_mut().pop();
    Ok(0)
  }

  fn visit_define(&mut self, name: &String, expr: &AstBox) -> Self::Result {
    // evaluate the expression
    let expr = self.visit(expr)?;
    // update the current environment
    if self.envs.borrow_mut().add(name.clone(), expr) {
      Ok(0)
    } else {
      Err("symbol has already been defined")
    }
  }

  fn visit_assign(&mut self, name: &String, expr: &AstBox) -> Self::Result {
    // evaluate the expression
    let expr = self.visit(expr)?;
    // update value of the symbol
    self
      .envs
      .borrow_mut()
      .update_until(name, expr, |map| map.contains_key::<String>(&RET_VAL))
      .then(|| 0)
      .ok_or("symbol has not been defined")
  }

  fn visit_if(&mut self, cond: &AstBox, then: &AstBox, else_then: &Option<AstBox>) -> Self::Result {
    // evaluate the condition
    let cond = self.visit(cond)?;
    // evaluate true/false branch
    if cond != 0 {
      self.visit(then)
    } else {
      else_then.as_ref().map_or(Ok(0), |ast| self.visit(&ast))
    }
  }

  fn visit_return(&mut self, expr: &AstBox) -> Self::Result {
    // evaluate the return value
    let expr = self.visit(expr)?;
    // update the current return value
    let succ = self.envs.borrow_mut().update_rec(&RET_VAL, expr);
    debug_assert!(succ, "environment corrupted");
    Ok(0)
  }

  fn visit_binary(&mut self, op: &Operator, lhs: &AstBox, rhs: &AstBox) -> Self::Result {
    // check if is logical operator
    match *op {
      Operator::LAnd | Operator::LOr => {
        // evaluate lhs first
        let lhs = self.visit(lhs)?;
        // check if need to evaluate rhs
        if (*op == Operator::LAnd && lhs == 0) || (*op == Operator::LOr && lhs != 0) {
          Ok(lhs)
        } else {
          self.visit(rhs)
        }
      }
      _ => {
        // evaluate the lhs & rhs
        let lhs = self.visit(lhs)?;
        let rhs = self.visit(rhs)?;
        // perform binary operation
        Ok(match *op {
          Operator::Add => lhs + rhs,
          Operator::Sub => lhs - rhs,
          Operator::Mul => lhs * rhs,
          Operator::Div => lhs / rhs,
          Operator::Mod => lhs % rhs,
          Operator::Less => (lhs < rhs) as i32,
          Operator::LessEq => (lhs <= rhs) as i32,
          Operator::Eq => (lhs == rhs) as i32,
          Operator::NotEq => (lhs != rhs) as i32,
          _ => panic!("unknown binary operator"),
        })
      }
    }
  }

  fn visit_unary(&mut self, op: &Operator, opr: &AstBox) -> Self::Result {
    // evaluate the operand
    let opr = self.visit(opr)?;
    // perform unary operation
    Ok(match *op {
      Operator::Sub => -opr,
      Operator::LNot => !opr,
      _ => panic!("invalid unary operator"),
    })
  }

  fn visit_funcall(&mut self, name: &String, args: &[AstBox]) -> Self::Result {
    // handle library function call
    if let Some(ret) = self.call_lib_func(name, args)? {
      return Ok(ret);
    }
    // find the specific function
    match self.funcs.clone().borrow().get(name) {
      Some(func) => {
        // make a new environment for arguments
        self.envs.borrow_mut().push();
        // evaluate arguments
        let (_, arg_names, _) = unwrap_struct!(func.as_ref(), Ast::FunDef, name, args, body);
        if arg_names.len() != args.len() {
          return Err("argument count mismatch");
        }
        for (arg, name) in args.iter().zip(arg_names.iter()) {
          // evaluate the current arguments
          let arg = self.visit(arg)?;
          // add to the current environment
          if !self.envs.borrow_mut().add(name.clone(), arg) {
            return Err("redifinition of argument");
          }
        }
        // call the specific function
        let ret = self.visit(func);
        // exit the current environment
        self.envs.borrow_mut().pop();
        ret
      }
      None => Err("function not found"),
    }
  }

  fn visit_int(&mut self, val: &i32) -> Self::Result {
    Ok(*val)
  }

  fn visit_id(&mut self, val: &String) -> Self::Result {
    // find in environment
    self
      .envs
      .borrow()
      .get_rec(val)
      .map_or(Err("symbol has not been defined"), |v| Ok(*v))
  }
}
