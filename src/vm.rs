use miette::{IntoDiagnostic, Result};
use std::{borrow::Cow, fmt::Debug};

use crate::{arena::Arena, errors::RuntimeError, interner::Interner, typedef::Chunk};

#[allow(non_camel_case_types, unused)]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    OP_RETURN,
    OP_NEGATE,

    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_CONSTANT,
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
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_LOCAL,
    OP_GET_LOCAL,
    OP_JUMP_IF_FALSE,
    OP_JUMP,
    OP_LOOP,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Number(f32),
    Bool(bool),
    Null,
    Symbol(u16),
    Str(u16),
}

impl Value {
    pub fn as_number(&self, line: usize) -> Result<f32> {
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

pub struct LineInfo {
    start: usize,
    line: usize,
}

pub struct VM {
    pub(crate) chunk: Chunk,
    pub(crate) arena: Arena,
    stack: Vec<Value>,
    values: Vec<Value>,
    // <Interned name: Valuees id>
    lines: Vec<LineInfo>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(1024),
            arena: Arena::new(),
            stack: Vec::with_capacity(256),
            values: Vec::with_capacity(256),
            lines: Vec::with_capacity(128),
        }
    }

    pub fn write_instruction(&mut self, instruction: OpCode, line: usize) {
        self.chunk.push(instruction as u8);

        if let Some(last) = self.lines.last()
            && last.line == line
        {
            return;
        }
        self.lines.push(LineInfo {
            start: self.chunk.len(),
            line,
        })
    }

    pub fn write_value(&mut self, value: u16, line: usize) {
        self.chunk.extend_from_slice(&value.to_le_bytes());
        for _ in 0..2 {
            self.lines.push(LineInfo {
                start: self.chunk.len(),
                line,
            });
        }
    }

    #[inline(always)]
    pub fn read_instruction(&mut self) -> OpCode {
        self.chunk.inc_ip(1);
        unsafe { std::mem::transmute::<u8, OpCode>(self.chunk.get_previous()) }
    }

    pub fn read_u16(&mut self) -> u16 {
        self.chunk.inc_ip(2);
        u16::from_le_bytes([self.chunk.get_byte_at_depth(2), self.chunk.get_previous()])
    }

    #[inline(always)]
    pub fn add_value(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    #[inline(always)]
    pub fn line_for_ip(&self, ip: usize) -> usize {
        let idx = self
            .lines
            .partition_point(|e| e.start <= ip)
            .saturating_sub(1);

        self.lines[idx].line
    }

    pub fn run<T: std::io::Write>(&mut self, interner: &Interner, writer: &mut T) -> Result<()> {
        while !self.chunk.is_at_end() {
            // #[cfg(debug_assertions)]
            // {
            //     self.debug(interner);
            //     std::thread::sleep(std::time::Duration::from_millis(
            //         std::env::args().collect::<Vec<String>>()[2]
            //             .parse::<u64>()
            //             .unwrap_or(0),
            //     ));
            // }
            let instruction = self.read_instruction();
            let line = self.line_for_ip(self.chunk.get_ip());

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
                OP_CONSTANT => {
                    let c = self.read_u16();
                    self.stack.push(self.values[c as usize]);
                }
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

                            Value::Str(self.arena.alloc_string(c))
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
                OP_PRINT => writeln!(
                    writer,
                    "{}",
                    self.stack.pop().unwrap().as_string(interner, &self.arena)
                )
                .into_diagnostic()?,
                OP_POP => {
                    let _ = self.stack.pop();
                }
                OP_DEFINE_GLOBAL => {
                    let v = self.read_u16();
                    let value = self.stack.pop().unwrap();
                    self.arena.globals.insert(v, value);
                }
                OP_GET_GLOBAL => {
                    let v = self.read_u16();
                    self.stack
                        .push(*self.arena.globals.get(&v).ok_or_else(|| RuntimeError {
                            advice: format!(
                                "undefined variable \"{}\" referenced on line {}.",
                                interner.resolve(v),
                                self.line_for_ip(self.chunk.get_ip())
                            ),
                        })?)
                }
                OP_SET_GLOBAL => {
                    let v = self.read_u16();
                    let value = self.stack.last().unwrap();
                    let key = self.arena.globals.get_mut(&v);

                    let Some(key) = key else {
                        return Err(RuntimeError {
                            advice: format!(
                                "undefined variable \"{}\" referenced on line {}.",
                                interner.resolve(v),
                                self.line_for_ip(self.chunk.get_ip())
                            ),
                        }
                        .into());
                    };

                    *key = *value
                }
                OP_SET_LOCAL => {
                    let slot = self.read_u16();
                    self.stack[slot as usize] = *self.stack.last().unwrap()
                }
                OP_GET_LOCAL => {
                    let slot = self.read_u16();
                    self.stack.push(self.stack[slot as usize])
                }
                OP_JUMP_IF_FALSE => {
                    let jump = self.read_u16();
                    if !self.stack.last().unwrap().as_bool() {
                        self.chunk.inc_ip(jump as usize);
                    }
                }
                OP_JUMP => {
                    let jump = self.read_u16();
                    self.chunk.inc_ip(jump as usize);
                }
                OP_LOOP => {
                    let jump = self.read_u16();
                    self.chunk.dec_ip(jump as usize);
                }
            }
        }

        Ok(())
    }
}
