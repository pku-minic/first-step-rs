use crate::define::Operator;
use std::io::{Error, ErrorKind, Result, Write};

/// Function definition.
pub struct FunctionDef {
  /// Function name.
  name: String,
  /// Argument number.
  arg_num: usize,
  /// Slot number.
  slot_num: usize,
  /// Instructions.
  /// `None` if is library function ('input' and 'print').
  insts: Option<Vec<InstBox>>,
}

/// Box for function definitions.
pub type FunDefBox = Box<FunctionDef>;

/// IR value.
pub enum Value {
  /// Stack slot.
  Slot { id: usize },
  /// Argument reference.
  ArgRef { id: usize },
  /// Label.
  Label { id: usize },
  /// Integer.
  Integer { val: i32 },
}

/// Box for IR values.
pub type ValBox = Box<Value>;

/// IR instruction.
pub enum Inst {
  /// Assignment.
  Assign { dest: ValBox, val: ValBox },

  /// Branch if not equal to zero.
  BranchNez { cond: ValBox, label: ValBox },

  /// Branch if equal to zero.
  BranchEqz { cond: ValBox, label: ValBox },

  /// Unconditional jump.
  Jump { label: ValBox },

  /// Label definition.
  Label { label: ValBox },

  /// Function call.
  Call {
    dest: ValBox,
    func: FunDefBox,
    args: Vec<ValBox>,
  },

  /// Function return.
  Return { val: ValBox },

  /// Binary operation.
  Binary {
    dest: ValBox,
    op: Operator,
    lhs: ValBox,
    rhs: ValBox,
  },

  /// Unary operation.
  Unary {
    dest: ValBox,
    op: Operator,
    opr: ValBox,
  },
}

/// Box for IR instructions.
pub type InstBox = Box<Inst>;

/// Register for storing the intermediate result.
const RESULT_REG: &str = "t0";

/// Register for storing the temporary data.
const TEMP_REG: &str = "t1";

impl FunctionDef {
  /// Creates a new function definition.
  pub fn new(name: String, arg_num: usize) -> Self {
    Self {
      name: name,
      arg_num: arg_num,
      slot_num: 0,
      insts: Some(Vec::new()),
    }
  }

  /// Creates a new library function declaration.
  pub fn new_lib(name: String) -> Self {
    Self {
      name: name,
      arg_num: 0,
      slot_num: 0,
      insts: None,
    }
  }

  /// Creates and pushes a new instruction to the current function,
  /// panics when pushing instructions to a library function declaration.
  pub fn push_inst(&mut self, inst: InstBox) {
    self.insts.as_mut().unwrap().push(inst)
  }

  /// Creates a new stack slot definition.
  pub fn add_slot(&mut self) -> ValBox {
    let slot = Box::new(Value::Slot { id: self.slot_num });
    self.slot_num += 1;
    slot
  }

  /// Dumps RISC-V assembly of the current function,
  /// panics when dumping a library function declaration.
  pub fn dump(&self, writer: &mut impl Write) -> Result<()> {
    // dump header
    writeln!(writer, "  .text")?;
    writeln!(writer, "  .globl {}", self.name)?;
    writeln!(writer, "{}:", self.name)?;
    // dump prologue
    writeln!(writer, "  addi sp, sp, -{}", self.slot_offset())?;
    writeln!(writer, "  sw ra, {}(sp)", self.slot_offset() - 4)?;
    if self.arg_num > 8 {
      return Err(Error::new(
        ErrorKind::Other,
        "argument count is greater than 8",
      ));
    }
    for i in 0..self.arg_num {
      writeln!(
        writer,
        "  sw s{}, {}(sp)",
        i,
        self.slot_offset() - 4 * (i + 2)
      )?;
      writeln!(writer, "  mv s{}, a{}", i, i)?;
    }
    // dump instructions
    for inst in self.insts.as_ref().unwrap() {
      dump_inst(writer, inst, self)?;
    }
    writeln!(writer, "")
  }

  /// Gets the slot offset.
  fn slot_offset(&self) -> usize {
    ((self.arg_num + self.slot_num) / 4 + 1) * 16
  }
}

/// Dumps RISC-V assembly of the specific instruction.
fn dump_inst(writer: &mut impl Write, inst: &InstBox, func: &FunctionDef) -> Result<()> {
  match inst.as_ref() {
    Inst::Assign { dest, val } => {
      dump_read(writer, val)?;
      dump_write(writer, dest)
    }
    Inst::BranchNez { cond, label } => {
      dump_read(writer, cond)?;
      write!(writer, "  bnez {}, ", RESULT_REG)?;
      dump_read(writer, label)?;
      writeln!(writer, "")
    }
    Inst::BranchEqz { cond, label } => {
      dump_read(writer, cond)?;
      write!(writer, "  beqz {}, ", RESULT_REG)?;
      dump_read(writer, label)?;
      writeln!(writer, "")
    }
    Inst::Jump { label } => {
      write!(writer, "  j ")?;
      dump_read(writer, label)?;
      writeln!(writer, "")
    }
    Inst::Label { label } => {
      dump_read(writer, label)?;
      writeln!(writer, ":")
    }
    Inst::Call { dest, func, args } => {
      // dump arguments
      if args.len() > 8 {
        return Err(Error::new(
          ErrorKind::Other,
          "argument count is greater than 8",
        ));
      }
      for (i, arg) in args.iter().enumerate() {
        dump_read(writer, arg)?;
        writeln!(writer, "  mv a{}, {}", i, RESULT_REG)?;
      }
      // dump function call
      writeln!(writer, "  call {}", func.name)?;
      writeln!(writer, "  mv {}, a0", RESULT_REG)?;
      dump_write(writer, dest)
    }
    Inst::Return { val } => {
      // dump return value
      dump_read(writer, val)?;
      writeln!(writer, "  mv a0, {}", RESULT_REG)?;
      // dump epilogue
      debug_assert!(func.arg_num <= 8, "argument count is greater than 8");
      for i in 0..func.arg_num {
        writeln!(
          writer,
          "  lw s{}, {}(sp)",
          i,
          func.slot_offset() - 4 * (i + 2)
        )?;
      }
      writeln!(writer, "  lw ra, {}(sp)", func.slot_offset() - 4)?;
      writeln!(writer, "  addi sp, sp, {}", func.slot_offset())?;
      writeln!(writer, "  ret")
    }
    Inst::Binary { dest, op, lhs, rhs } => {
      // dump lhs & rhs
      dump_read(writer, lhs)?;
      writeln!(writer, "  mv {}, {}", TEMP_REG, RESULT_REG)?;
      dump_read(writer, rhs)?;
      // perform binary operation
      match op {
        Operator::LessEq => {
          writeln!(writer, "  sgt {}, {}, {}", RESULT_REG, TEMP_REG, RESULT_REG)?;
          writeln!(writer, "  seqz {}, {}", RESULT_REG, RESULT_REG)?;
        }
        Operator::Eq | Operator::NotEq => {
          writeln!(writer, "  xor {}, {}, {}", RESULT_REG, TEMP_REG, RESULT_REG)?;
          writeln!(
            writer,
            "  s{}z {}, {}",
            (if *op == Operator::Eq { "eq" } else { "ne" }),
            RESULT_REG,
            RESULT_REG
          )?;
        }
        _ => {
          writeln!(
            writer,
            "  {} {}, {}, {}",
            match op {
              Operator::Add => "add",
              Operator::Sub => "sub",
              Operator::Mul => "mul",
              Operator::Div => "div",
              Operator::Mod => "rem",
              Operator::Less => "slt",
              _ => panic!("unknown binary operator"),
            },
            RESULT_REG,
            TEMP_REG,
            RESULT_REG
          )?;
        }
      }
      // store the result to dest
      dump_write(writer, dest)
    }
    Inst::Unary { dest, op, opr } => {
      // dump operand
      dump_read(writer, opr)?;
      // perform unary operation
      writeln!(
        writer,
        "  {} {}, {}",
        match op {
          Operator::Sub => "neg",
          Operator::LNot => "seqz",
          _ => panic!("unknown unary operator"),
        },
        RESULT_REG,
        RESULT_REG
      )?;
      // store the result to dest
      dump_write(writer, dest)
    }
  }
}

/// Dumps RISC-V assembly of reading the specific value.
fn dump_read(writer: &mut impl Write, val: &ValBox) -> Result<()> {
  match val.as_ref() {
    Value::Slot { id } => writeln!(writer, "  lw {}, {}(sp)", RESULT_REG, id * 4),
    Value::ArgRef { id } => writeln!(writer, "  mv {}, s{}", RESULT_REG, id),
    Value::Label { id } => write!(writer, ".label{}", id),
    Value::Integer { val } => writeln!(writer, "  li {}, {}", RESULT_REG, val),
  }
}

/// Dumps RISC-V assembly of writing the specific value.
fn dump_write(writer: &mut impl Write, val: &ValBox) -> Result<()> {
  match val.as_ref() {
    Value::Slot { id } => writeln!(writer, "  sw {}, {}(sp)", RESULT_REG, id * 4),
    Value::ArgRef { id } => writeln!(writer, "  mv s{}, {}", id, RESULT_REG),
    Value::Label { .. } => panic!("writing a label"),
    Value::Integer { .. } => panic!("writing an integer"),
  }
}
