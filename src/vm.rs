use miette::{IntoDiagnostic, Result};
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
    pub(crate) arena: Arena,
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

    #[inline(always)]
    pub fn read_instruction(&mut self) -> &OpCode {
        let old = &self.instructions[self.ip];
        self.ip += 1;
        old
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
        while self.ip < self.instructions.len() {
            // #[cfg(debug_assertions)]
            // {
            //     self.debug(interner);
            //     std::thread::sleep(std::time::Duration::from_millis(
            //         std::env::args().collect::<Vec<String>>()[2]
            //             .parse::<u64>()
            //             .unwrap_or(0),
            //     ));
            // }
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
                    let value = self.stack.last().unwrap();
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

                    *key = *value
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
        print!("\x1B[2J\x1B[H");
        use std::io::{self, Write};
        io::stdout().flush().unwrap();
        use OpCode::*;
        use Value::*;

        println!("\n========== VM DEBUG ==========\n");

        println!("Stack (top last):");
        if self.stack.is_empty() {
            println!("\n\n\n\n\n\n\n  <empty>");
        } else {
            for (i, v) in self.stack.iter().enumerate() {
                println!("  [{:02}] {:?}", i, v);
            }
        }

        println!("\nConstants:");
        if self.values.is_empty() {
            println!("  <none>");
        } else {
            for (i, v) in self.values.iter().enumerate() {
                let rendered = match v {
                    Number(n) => n.to_string(),
                    Bool(b) => b.to_string(),
                    Null => "null".into(),
                    Symbol(s) => interner.resolve(*s).into(),
                    Str(s) => self.arena.get_string(*s).into(),
                };
                println!("  #{:03} {:?} = {}", i, v, rendered);
            }
        }

        println!("\nArena strings:");
        if self.arena.strings.is_empty() {
            println!("  <none>");
        } else {
            for (i, s) in self.arena.strings.iter().enumerate() {
                println!("  @{i:03} \"{s}\"");
            }
        }

        println!("\nInstructions:");
        println!(" idx | line | ip→ | opcode                     | meaning");
        println!("-----+------+-----+----------------------------+-------------------------------");

        for (i, instr) in self.instructions.iter().enumerate() {
            let line = self.line_for_ip(i);
            let ip_marker = if i == self.ip { "▶" } else { " " };

            print!(
                "{:04} | {:04} |  {}  | {:<26} | ",
                i,
                line,
                ip_marker,
                format!("{instr:?}")
            );

            match *instr {
                OP_RETURN => println!("halt execution, dump stack"),
                OP_NEGATE => println!("pop number → push (-value)"),
                OP_NOT => println!("pop value → push logical negation"),
                OP_NULL => println!("push null"),
                OP_TRUE => println!("push true"),
                OP_FALSE => println!("push false"),
                OP_CONSTANT(idx) => {
                    let v = self.values[idx as usize];
                    println!("push constant #{idx} ({v:?})");
                }
                OP_ADD => println!("pop a, pop b → push (b + a) or string concat"),
                OP_SUBTRACT => println!("pop a, pop b → push (b - a)"),
                OP_MULTIPLY => println!("pop a, pop b → push (b * a)"),
                OP_DIVIDE => println!("pop a, pop b → push (b / a)"),
                OP_EQUAL => println!("pop a, pop b → push (b == a)"),
                OP_NOT_EQUAL => println!("pop a, pop b → push (b != a)"),
                OP_GREATER => println!("pop a, pop b → push (b > a)"),
                OP_LESS => println!("pop a, pop b → push (b < a)"),
                OP_GREATER_EQUAL => println!("pop a, pop b → push (b ≥ a)"),
                OP_LESS_EQUAL => println!("pop a, pop b → push (b ≤ a)"),
                OP_PRINT => println!("pop value → print"),
                OP_POP => println!("discard top of stack"),
                OP_DEFINE_GLOBAL(sym) => {
                    println!("pop value → define global '{}'", interner.resolve(sym))
                }
                OP_GET_GLOBAL(sym) => println!("push global '{}'", interner.resolve(sym)),
                OP_SET_GLOBAL(sym) => {
                    println!("pop value → assign global '{}'", interner.resolve(sym))
                }
                OP_GET_LOCAL(slot) => println!("push local slot {slot}"),
                OP_SET_LOCAL(slot) => println!("assign local slot {slot} from top of stack"),
                OP_JUMP_IF_FALSE(offset) => {
                    let target = i + offset as usize;
                    println!("if top is false → ip += {offset} (→ {target})");
                }
                OP_JUMP(offset) => {
                    let target = i + offset as usize;
                    println!("unconditional jump +{offset} (→ {target})");
                }
                OP_LOOP(offset) => {
                    let target = i - offset as usize;
                    println!("loop back −{offset} (→ {target})");
                }
            }
        }

        println!("\n========== END DEBUG ==========\n");
    }
}
