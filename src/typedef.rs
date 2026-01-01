use std::{
    num::{NonZero, NonZeroU32},
    ops::Range,
};

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
    pub symbol: Option<StringRef>,
}

impl Lexeme {
    pub fn new(
        ty: Token,
        line_n: u32,
        start: usize,
        end: usize,
        symbol: Option<StringRef>,
    ) -> Self {
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

pub type StringRef = NonZero<u32>;

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
    pub(crate) name: u32,
    depth: Option<NonZeroU32>,
}

impl Local {
    pub fn get_depth(&self) -> Option<u32> {
        self.depth.map(|v| v.get() - 1)
    }

    pub fn set_depth(&mut self, val: u32) {
        self.depth = NonZeroU32::new(val + 1);
    }
}

#[derive(Debug, Default)]
pub struct ScopeManager {
    pub(crate) locals: Vec<Local>,
    pub local_count: usize,
    pub scope_depth: u32,
}

impl ScopeManager {
    pub fn new() -> Self {
        ScopeManager {
            ..Default::default()
        }
    }

    pub fn get(&self, i: usize) -> &Local {
        self.locals
            .get(i)
            .expect("attempted to get uninitialised local")
    }

    pub fn mark_initialised(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        self.locals
            .get_mut(self.local_count - 1)
            .expect("attempted to mutate uninitialised local")
            .set_depth(self.scope_depth);
    }

    pub fn add_uninitialised_local(&mut self, name: u32) {
        self.locals.push(Local { name, depth: None });
        self.local_count += 1;
    }

    pub fn pop(&mut self) {
        self.locals.pop();
        self.local_count -= 1;
    }

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

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
