use crate::define::{Ast, AstBox, AstVisitor};
use crate::define::{NestedMap, NestedMapPtr, Operator};
use crate::{ok_or_return, unwrap_struct};
use lazy_static::lazy_static;
use std::collections::HashMap;

/// Interpreter for `first-step` language.
pub struct Interpreter {
  /// All function definitions.
  funcs: HashMap<String, AstBox>,
  /// Environments.
  envs: NestedMapPtr<String, i32>,
}

/// `Result` for `Interpreter`.
pub type Result = std::result::Result<i32, &'static str>;

impl Interpreter {
  /// Creates a new interpreter.
  pub fn new() -> Self {
    Self {
      funcs: HashMap::new(),
      envs: NestedMap::new(),
    }
  }

  /// Adds the specific function definition to interpreter,
  /// returns false if failed.
  pub fn add_func_def(&mut self, func: AstBox) -> bool {
    match func.as_ref() {
      // get function name
      Ast::FunDef { name, .. } => {
        // check if is already defined
        if !self.funcs.contains_key(name) {
          // add function definition
          self.funcs.insert(name.clone(), func);
          true
        } else {
          false
        }
      }
      _ => panic!("not a function"),
    }
  }

  /// Evaluates the current program.
  pub fn eval(&mut self) -> Result {
    // find & evaluate the `main` function
    match self.funcs.get("main") {
      Some(main) => self.visit(main),
      _ => Err("'main' function not found"),
    }
  }

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
          let arg = ok_or_return!(self.visit(args.first().unwrap()));
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

lazy_static! {
  /// Name of return value when evaluating.
  static ref RET_VAL: String = "$ret".to_string();
}

impl AstVisitor for Interpreter {
  type Result = Result;

  fn visit_fundef(&mut self, _name: &String, _args: &[String], body: &AstBox) -> Self::Result {
    // set up the default return value
    let ret = self.envs.add(RET_VAL.clone(), 0);
    assert_eq!(ret, true, "environment corrupted");
    // evaluate function body
    ok_or_return!(self.visit(body));
    // get return value
    Ok(*self.envs.get(&RET_VAL, false).unwrap())
  }

  fn visit_block(&mut self, stmts: &[AstBox]) -> Self::Result {
    // enter a new environment
    self.envs = NestedMap::new_with_outer(self.envs);
    // evaluate all statements
    for stmt in stmts.iter() {
      ok_or_return!(self.visit(stmt));
    }
    // exit the current environment
    self.envs = self.envs.outer();
    Ok(0)
  }

  fn visit_define(&mut self, name: &String, expr: &AstBox) -> Self::Result {
    // evaluate the expression
    let expr = ok_or_return!(self.visit(expr));
    // update the current environment
    if self.envs.add(name.clone(), expr) {
      Ok(0)
    } else {
      Err("symbol has already been defined")
    }
  }

  fn visit_assign(&mut self, name: &String, expr: &AstBox) -> Self::Result {
    // evaluate the expression
    let expr = ok_or_return!(self.visit(expr));
    // update value of the symbol
    let mut envs = &mut self.envs;
    let mut succ = false;
    while !envs.is_root() {
      // try to update the value
      if envs.update(name, expr, false) {
        succ = true;
        break;
      }
      // do not cross the boundary of function
      if envs.get(&RET_VAL, false).is_some() {
        break;
      }
      // enter the outer environment
      envs = envs.outer_mut();
    }
    // check if success
    if succ {
      Ok(0)
    } else {
      Err("symbol has not been defined")
    }
  }

  fn visit_if(&mut self, cond: &AstBox, then: &AstBox, else_then: &Option<AstBox>) -> Self::Result {
    // evaluate the condition
    let cond = ok_or_return!(self.visit(cond));
    // evaluate true/false branch
    if cond != 0 {
      self.visit(then)
    } else {
      else_then.as_ref().map_or(Ok(0), |ast| self.visit(&ast))
    }
  }

  fn visit_return(&mut self, expr: &AstBox) -> Self::Result {
    // evaluate the return value
    let expr = ok_or_return!(self.visit(expr));
    // update the current return value
    let succ = self.envs.update_rec(&RET_VAL, expr);
    assert_eq!(succ, true, "environment corrupted");
    Ok(0)
  }

  fn visit_binary(&mut self, op: &Operator, lhs: &AstBox, rhs: &AstBox) -> Self::Result {
    // check if is logical operator
    match *op {
      Operator::LAnd | Operator::LOr => {
        // evaluate lhs first
        let lhs = ok_or_return!(self.visit(lhs));
        // check if need to evaluate rhs
        if (*op == Operator::LAnd && lhs == 0) || (*op == Operator::LOr && lhs != 0) {
          Ok(lhs)
        } else {
          self.visit(rhs)
        }
      }
      _ => {
        // evaluate the lhs & rhs
        let lhs = ok_or_return!(self.visit(lhs));
        let rhs = ok_or_return!(self.visit(rhs));
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
    let opr = ok_or_return!(self.visit(opr));
    // perform unary operation
    Ok(match *op {
      Operator::Sub => -opr,
      Operator::LNot => !opr,
      _ => panic!("invalid unary operator"),
    })
  }

  fn visit_funcall(&mut self, name: &String, args: &[AstBox]) -> Self::Result {
    // handle library function call
    if let Some(ret) = ok_or_return!(self.call_lib_func(name, args)) {
      return Ok(ret);
    }
    // find the specific function
    let func = match self.funcs.get(name) {
      Some(func) => func,
      None => return Err("function not found"),
    };
    // make a new environment for arguments
    self.envs = NestedMap::new_with_outer(self.envs);
    // evaluate arguments
    let (_, arg_names, _) = unwrap_struct!(func.as_ref(), Ast::FunDef, name, args, body);
    if arg_names.len() != args.len() {
      return Err("argument count mismatch");
    }
    for (arg, name) in args.iter().zip(arg_names.iter()) {
      // evaluate the current arguments
      let arg = ok_or_return!(self.visit(arg));
      // add to the current environment
      if !self.envs.add(name.clone(), arg) {
        return Err("redifinition of argument");
      }
    }
    // call the specific function
    let ret = self.visit(func);
    // exit the current environment
    self.envs = self.envs.outer();
    ret
  }

  fn visit_int(&mut self, val: &i32) -> Self::Result {
    Ok(*val)
  }

  fn visit_id(&mut self, val: &String) -> Self::Result {
    // find in environment
    self
      .envs
      .get_rec(val)
      .map_or(Err("symbol has not been defined"), |v| Ok(*v))
  }
}
