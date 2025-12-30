use miette::Result;
use std::{borrow::Cow, fmt::Debug};

use crate::{arena::Arena, errors::RuntimeError, interner::Interner};

#[allow(non_camel_case_types, unused)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OP_RETURN,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_CONSTANT(u32),
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

    OP_PRINT,
    OP_POP,
    OP_DEFINE_GLOBAL(u32),
    OP_SET_GLOBAL(u32),
    OP_GET_GLOBAL(u32),

    OP_SET_LOCAL(u32),
    OP_GET_LOCAL(u32),

    OP_JUMP_IF_FALSE(u32),
    OP_JUMP(u32),
    OP_LOOP(u32),
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
    Symbol(u32),
    Str(u32),
}

impl Value {
    pub fn as_number(&self, line: usize) -> Result<f64> {
        match self {
            Self::Number(v) => Ok(*v),
            Self::Bool(true) => Ok(1.),
            Self::Bool(false) => Ok(0.),
            _ => Err(RuntimeError {
                advice: format!("attempted to convert '{self:?}' to a number on line {line}",),
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

    pub fn as_string<'a>(&self, interner: &'a Interner, arena: &'a Arena) -> Cow<'a, str> {
        match self {
            Self::Symbol(a) => Cow::Borrowed(interner.resolve(*a)),
            Self::Str(a) => Cow::Borrowed(arena.get_string(*a)),
            Self::Null => Cow::Borrowed("null"),
            Self::Bool(v) => match v {
                true => Cow::Borrowed("true"),
                false => Cow::Borrowed("false"),
            },
            Self::Number(v) => Cow::Owned(v.to_string()),
        }
    }
}

#[derive(Clone)]
pub struct LineInfo {
    start: usize,
    line: usize,
}

#[derive(Clone)]
pub struct VM {
    ip: usize,
    arena: Arena,
    stack: Vec<Value>,
    pub(crate) instructions: Vec<OpCode>,
    values: Vec<Value>,
    // <Interned name: Valuees id>
    lines: Vec<LineInfo>,
}

impl VM {
    pub fn new() -> Self {
        let instructions = Vec::new();
        Self {
            ip: 0,
            instructions,
            arena: Arena::new(),
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

    pub fn run(&mut self, interner: &Interner) -> Result<()> {
        while self.ip < self.instructions.len() {
            let instruction = *self.read_instruction();
            let line = self.line_for_ip(self.ip);

            macro_rules! binop {
            ($stack:expr, $wrap:ident, $op:tt) => {{
                let a = $stack.pop().unwrap().as_number(line)?;
                let b = $stack.pop().unwrap().as_number(line)?;
                $stack.push(Value::$wrap(b $op a));
            }};
        }

            use self::{OpCode::*, Value::*};
            match instruction {
                OP_RETURN => {
                    println!("{:?}", self.stack);
                }
                OP_NEGATE => {
                    let m = -self.stack.pop().unwrap().as_number(line)?;
                    self.stack.push(Number(m));
                }
                OP_NOT => {
                    let m = !self.stack.pop().unwrap().as_bool();
                    self.stack.push(Bool(m));
                }
                OP_NULL => self.stack.push(Null),
                OP_TRUE => self.stack.push(Bool(true)),
                OP_FALSE => self.stack.push(Bool(false)),
                OP_CONSTANT(c) => self.stack.push(self.values[c as usize]),
                OP_ADD => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    self.stack.push(match (a, b) {
                        (Symbol(_), _) | (Str(_), _) => {
                            let (a, b) = (
                                a.as_string(interner, &self.arena),
                                b.as_string(interner, &self.arena),
                            );
                            let mut c = String::with_capacity(a.len() + b.len());
                            c.push_str(&a);
                            c.push_str(&b);

                            Value::Str(self.arena.alloc_string(c).get())
                        }
                        _ => Value::Number(a.as_number(line)? + b.as_number(line)?),
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
                OP_PRINT => println!(
                    "{}",
                    self.stack.pop().unwrap().as_string(interner, &self.arena)
                ),
                OP_POP => {
                    let _ = self.stack.pop();
                }
                OP_DEFINE_GLOBAL(v) => {
                    let value = self.stack.pop().unwrap();
                    self.arena.globals.insert(v, value);
                }
                OP_GET_GLOBAL(v) => {
                    self.stack
                        .push(*self.arena.globals.get(&v).ok_or_else(|| RuntimeError {
                            advice: format!(
                                "undefined variable \"{}\" referenced on line {}.",
                                interner.resolve(v),
                                self.line_for_ip(self.ip)
                            ),
                        })?)
                }
                OP_SET_GLOBAL(v) => {
                    let value = self.stack.pop().unwrap();
                    let key = self.arena.globals.get_mut(&v);

                    let Some(key) = key else {
                        return Err(RuntimeError {
                            advice: format!(
                                "undefined variable \"{}\" referenced on line {}.",
                                interner.resolve(v),
                                self.line_for_ip(self.ip)
                            ),
                        }
                        .into());
                    };

                    *key = value
                }
                OP_SET_LOCAL(slot) => self.stack[slot as usize] = *self.stack.last().unwrap(),
                OP_GET_LOCAL(slot) => self.stack.push(self.stack[slot as usize]),
                OP_JUMP_IF_FALSE(jump) => {
                    if !self.stack.last().unwrap().as_bool() {
                        self.ip += jump as usize
                    }
                }
                OP_JUMP(jump) => self.ip += jump as usize,
                OP_LOOP(jump) => self.ip -= jump as usize,
            }
        }

        Ok(())
    }
}

#[cfg(debug_assertions)]
impl VM {
    pub fn debug(&self, interner: &Interner) {
        println!("Stack: {:?}", self.stack);
        println!("Values: ");

        for value in self.values.iter() {
            println!(
                "   {:?}: {}",
                value,
                match value {
                    Value::Number(b) => b.to_string(),
                    Value::Bool(v) => v.to_string(),
                    Value::Null => "null".to_owned(),
                    Value::Symbol(a) => interner.resolve(*a).to_string(),
                    Value::Str(a) => self.arena.get_string(*a).to_string(),
                }
            )
        }

        println!("Arena: ");

        for (i, val) in self.arena.strings.iter().enumerate() {
            println!("   {:?}: {}", i, val)
        }

        println!("Instructions:");

        for (i, instruction) in self.instructions.iter().enumerate() {
            print!("    ");
            match instruction.is_simple() {
                true => println!("{:0>4}L{:0>4} {:?} ", i, self.line_for_ip(i), instruction),
                false => match instruction {
                    v @ OpCode::OP_CONSTANT(x) => println!(
                        "{:0>4}L{:0>4} {:?} -> {:?} ",
                        i,
                        self.line_for_ip(i),
                        v,
                        match self.values[*x as usize] {
                            Value::Number(b) => b.to_string(),
                            Value::Bool(v) => v.to_string(),
                            Value::Null => "null".to_owned(),
                            Value::Symbol(a) => interner.resolve(a).to_string(),
                            Value::Str(a) => self.arena.get_string(a).to_string(),
                        },
                    ),
                    _ => unreachable!(),
                },
            }
        }

        println!()
    }
}
