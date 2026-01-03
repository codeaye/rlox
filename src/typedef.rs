use std::{num::NonZeroU16, ops::Range};

use miette::SourceSpan;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[rustfmt::skip]
#[repr(u8)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
pub enum Token {
  // Single-character tokens.
  LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  // Literals.
  IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NULL, OR,
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

  EOF,

  IGNORABLE,
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lexeme {
    pub ty: Token,
    pub line_n: u32,
    pub start: u32,
    pub len: u16,
    pub symbol: ZeroOptU16,
}

impl Lexeme {
    pub fn new(ty: Token, line_n: u32, start: usize, end: usize, symbol: ZeroOptU16) -> Self {
        Self {
            ty,
            line_n,
            start: start as u32,
            len: (end - start) as u16,
            symbol,
        }
    }

    pub fn find<'a>(&self, source: &'a str) -> &'a str {
        &source[self.as_range()]
    }

    pub fn as_range(&self) -> Range<usize> {
        (self.start as usize)..(self.start + self.len as u32) as usize
    }
}

impl From<&Lexeme> for SourceSpan {
    fn from(val: &Lexeme) -> Self {
        SourceSpan::from((val.start as usize, val.len as usize))
    }
}

#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum Precedence {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY,
}
impl Precedence {
    pub fn next_higher(&self) -> Self {
        // for right-associated parsing
        use Precedence::*;
        match self {
            PREC_NONE => PREC_ASSIGNMENT,
            PREC_ASSIGNMENT => PREC_OR,
            PREC_OR => PREC_AND,
            PREC_AND => PREC_EQUALITY,
            PREC_EQUALITY => PREC_COMPARISON,
            PREC_COMPARISON => PREC_TERM,
            PREC_TERM => PREC_FACTOR,
            PREC_FACTOR => PREC_UNARY,
            PREC_UNARY => PREC_CALL,
            PREC_CALL => PREC_PRIMARY,
            PREC_PRIMARY => PREC_PRIMARY,
        }
    }
}

#[derive(Debug)]
pub struct Local {
    pub(crate) name: u16,
    depth: ZeroOptU16,
}

impl Local {
    #[inline(always)]
    pub fn get_depth(&self) -> Option<u16> {
        self.depth.get()
    }
    #[inline(always)]
    pub fn set_depth(&mut self, val: u16) {
        self.depth = ZeroOptU16::new(val);
    }
}

#[derive(Debug, Default)]
pub struct ScopeManager {
    pub(crate) locals: Vec<Local>,
    pub local_count: usize,
    pub scope_depth: u16,
}

impl ScopeManager {
    #[inline(always)]
    pub fn new() -> Self {
        ScopeManager {
            ..Default::default()
        }
    }

    #[inline(always)]
    pub fn get(&self, i: usize) -> &Local {
        self.locals
            .get(i)
            .expect("attempted to get uninitialised local")
    }

    #[inline(always)]
    pub fn mark_initialised(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        self.locals
            .get_mut(self.local_count - 1)
            .expect("attempted to mutate uninitialised local")
            .set_depth(self.scope_depth);
    }

    #[inline(always)]
    pub fn add_uninitialised_local(&mut self, name: u16) {
        self.locals.push(Local {
            name,
            depth: ZeroOptU16::none(),
        });
        self.local_count += 1;
    }

    #[inline(always)]
    pub fn pop(&mut self) {
        self.locals.pop();
        self.local_count -= 1;
    }

    #[inline(always)]
    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    #[inline(always)]
    pub fn end_scope(&mut self) {
        self.scope_depth -= 1;
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParseFn {
    None,
    Grouping,
    Unary,
    Binary,
    Number,
    String,
    Literal,
    Variable,
    And,
    Or,
}

// (prefix, infix, precedence)
pub type ParseRule = (ParseFn, ParseFn, Precedence);

pub fn parse_token_to_rule(token: Token) -> ParseRule {
    use self::{ParseFn::*, Token::*};
    use crate::typedef::Precedence::*;

    match token {
        LEFT_PAREN => (Grouping, None, PREC_NONE),
        NUMBER => (Number, None, PREC_NONE),
        STRING => (String, None, PREC_NONE),
        MINUS => (Unary, Binary, PREC_TERM),
        PLUS => (None, Binary, PREC_TERM),
        SLASH | STAR => (None, Binary, PREC_FACTOR),
        FALSE | TRUE | NULL => (Literal, None, PREC_NONE),
        BANG => (Unary, None, PREC_UNARY),
        BANG_EQUAL | EQUAL_EQUAL => (None, Binary, PREC_EQUALITY),
        GREATER | GREATER_EQUAL | LESS | LESS_EQUAL => (None, Binary, PREC_COMPARISON),
        IDENTIFIER => (Variable, None, PREC_NONE),
        AND => (None, And, PREC_AND),
        OR => (None, Or, PREC_OR),
        _ => (None, None, PREC_NONE),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
#[repr(transparent)]
pub struct ZeroOptU16(Option<NonZeroU16>);

impl ZeroOptU16 {
    #[inline(always)]
    pub fn new(value: u16) -> Self {
        if value == u16::MAX {
            panic!("value too large for ZeroOptU16");
        }
        Self(Some(NonZeroU16::new(value + 1).unwrap()))
    }

    #[inline(always)]
    pub const fn none() -> Self {
        Self(None)
    }

    #[inline(always)]
    pub fn is_some(&self) -> bool {
        self.0.is_some()
    }

    #[inline(always)]
    pub fn is_none(&self) -> bool {
        self.0.is_none()
    }

    #[inline(always)]
    pub fn get(&self) -> Option<u16> {
        self.0.map(|nz| nz.get() - 1)
    }

    #[inline(always)]
    pub fn unwrap(&self) -> u16 {
        self.get().expect("ZeroOptU16 is None")
    }

    #[inline(always)]
    pub fn set(&mut self, value: u16) {
        if value == u16::MAX {
            panic!("value too large for ZeroOptU16");
        }
        self.0 = Some(NonZeroU16::new(value + 1).unwrap());
    }

    #[inline(always)]
    pub fn clear(&mut self) {
        self.0 = None;
    }
}

#[derive(Debug)]
pub struct Chunk {
    bytes: Vec<u8>,
    ip: usize,
}

impl Chunk {
    pub fn new(capacity: usize) -> Self {
        Self {
            bytes: Vec::with_capacity(capacity),
            ip: 0,
        }
    }

    #[inline(always)]
    pub fn inc_ip(&mut self, inc: usize) {
        self.ip += inc
    }

    #[inline(always)]
    pub fn dec_ip(&mut self, inc: usize) {
        self.ip -= inc
    }

    #[inline(always)]
    pub fn get_byte_at_depth(&self, offset: usize) -> u8 {
        self.bytes[self.ip - offset]
    }

    #[inline(always)]
    pub fn get_previous(&self) -> u8 {
        self.bytes[self.ip - 1]
    }

    #[inline(always)]
    pub fn get_ip(&self) -> usize {
        self.ip
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    #[inline(always)]
    pub fn push(&mut self, value: u8) {
        self.bytes.push(value);
    }

    #[inline(always)]
    pub fn set(&mut self, id: usize, value: u8) {
        self.bytes[id] = value;
    }

    #[inline(always)]
    pub fn extend_from_slice(&mut self, value: &[u8]) {
        self.bytes.extend_from_slice(value);
    }

    #[inline(always)]
    pub fn is_at_end(&self) -> bool {
        self.ip >= self.bytes.len()
    }
}
