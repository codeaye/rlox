use miette::Result;
use std::fmt::Debug;

use crate::{errors::UnsupportedTypeConversion, interner::Interner};

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OP_RETURN,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_CONSTANT(usize),
    OP_NULL,
    OP_TRUE,
    OP_FALSE,
    OP_NOT,
    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_GREATER_EQUAL,
    OP_LESS_EQUAL,
}

impl OpCode {
    #[cfg(debug_assertions)]
    pub fn is_simple(&self) -> bool {
        !matches!(self, OpCode::OP_CONSTANT(_))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Null,
    Symbol(usize),
}

impl Value {
    pub fn as_number(&self) -> Result<f64> {
        match self {
            Self::Number(v) => Ok(*v),
            Self::Bool(true) => Ok(1.),
            Self::Bool(false) => Ok(0.),
            _ => Err(UnsupportedTypeConversion {
                advice: format!("cannot convert '{self:?}' to a number"),
            }
            .into()),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Self::Bool(v) => *v,
            Self::Null => false,
            _ => true,
        }
    }
}

pub struct LineInfo {
    start: usize,
    line: usize,
}

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
    instructions: Vec<OpCode>,
    values: Vec<Value>,
    lines: Vec<LineInfo>,
}

impl VM {
    pub fn new() -> Self {
        let instructions = Vec::new();
        Self {
            ip: 0,
            instructions,
            stack: Vec::with_capacity(256),
            values: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instruction: OpCode, line: usize) {
        self.instructions.push(instruction);

        if let Some(last) = self.lines.last()
            && last.line == line
        {
            return;
        }
        self.lines.push(LineInfo {
            start: self.instructions.len(),
            line,
        })
    }

    pub fn read_instruction(&mut self) -> &OpCode {
        let old = &self.instructions[self.ip];
        self.ip += 1;
        old
    }

    pub fn add_value(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn line_for_ip(&self, ip: usize) -> usize {
        let idx = self
            .lines
            .partition_point(|e| e.start <= ip)
            .saturating_sub(1);

        self.lines[idx].line
    }

    // pub fn peek_value(&self, distance: usize) -> &Value {
    //     &self.values[self.values.len() - 1 - distance]
    // }

    pub fn run(&mut self, interner: &mut Interner) -> Result<()> {
        macro_rules! binop {
            ($stack:expr, $wrap:ident, $op:tt) => {{
                let a = $stack.pop().unwrap().as_number()?;
                let b = $stack.pop().unwrap().as_number()?;
                $stack.push(Value::$wrap(b $op a));
            }};
        }

        while self.ip < self.instructions.len() {
            let instruction = *self.read_instruction();
            use self::{OpCode::*, Value::*};
            match instruction {
                OP_RETURN => {
                    println!("{:?}", self.stack);
                }
                OP_NEGATE => {
                    let m = -self.stack.pop().unwrap().as_number()?;
                    self.stack.push(Number(m));
                }
                OP_NOT => {
                    let m = !self.stack.pop().unwrap().as_bool();
                    self.stack.push(Bool(m));
                }

                OP_NULL => self.stack.push(Null),
                OP_TRUE => self.stack.push(Bool(true)),
                OP_FALSE => self.stack.push(Bool(false)),
                OP_CONSTANT(c) => self.stack.push(self.values[c]),

                OP_ADD => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match (a, b) {
                        (Symbol(a), Symbol(b)) => {
                            let (mut a, b) = (
                                interner.resolve(a).unwrap().clone(),
                                interner.resolve(b).unwrap(),
                            );
                            a.push_str(b);
                            Value::Symbol(interner.intern(a.to_string()))
                        }
                        _ => Value::Number(a.as_number()? + b.as_number()?),
                    });
                }
                OP_SUBTRACT => binop!(self.stack, Number,  -),
                OP_MULTIPLY => binop!(self.stack, Number,  *),
                OP_DIVIDE => binop!(self.stack, Number,  /),
                OP_EQUAL => binop!(self.stack, Bool, ==),
                OP_NOT_EQUAL => binop!(self.stack, Bool, !=),
                OP_GREATER => binop!(self.stack, Bool, >),
                OP_LESS => binop!(self.stack, Bool, <),
                OP_GREATER_EQUAL => binop!(self.stack, Bool, >=),
                OP_LESS_EQUAL => binop!(self.stack, Bool, <=),
            }
        }

        Ok(())
    }
}

#[cfg(debug_assertions)]
impl Debug for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Stack: {:?}\n", self.stack))?;
        for (i, instruction) in self.instructions.iter().enumerate() {
            match instruction.is_simple() {
                true => f.write_fmt(format_args!(
                    "{:0>4}L{:0>4} {:?} \n",
                    i,
                    self.line_for_ip(i),
                    instruction,
                ))?,
                false => match instruction {
                    v @ OpCode::OP_CONSTANT(x) => f.write_fmt(format_args!(
                        "{:0>4}L{:0>4} {:?} -> {:?} \n",
                        i,
                        self.line_for_ip(i),
                        v,
                        match self.values[*x] {
                            Value::Number(b) => b,
                            _ => unreachable!(),
                        },
                    ))?,
                    _ => unreachable!(),
                },
            }
        }

        f.write_str("\n")
    }
}
