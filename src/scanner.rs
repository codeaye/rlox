use miette::{Result, SourceSpan};
use std::fmt::Debug;

use crate::{arena::StringRef, errors::CompileTimeError, interner::Interner};

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

  EOF
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
        line_n: usize,
        start: usize,
        end: usize,
        symbol: Option<StringRef>,
    ) -> Self {
        Self {
            ty,
            line_n: line_n as u32,
            start: start as u32,
            len: (end - start) as u16,
            symbol,
        }
    }

    pub fn find<'a>(&self, source: &'a str) -> &'a str {
        &source[(self.start as usize)..(self.start as usize + self.len as usize)]
    }
}

impl From<&Lexeme> for SourceSpan {
    fn from(val: &Lexeme) -> Self {
        SourceSpan::from((val.start as usize, val.len as usize))
    }
}

pub struct Scanner<'a> {
    source_str: &'a str,
    selection_start: usize,
    selection_end: usize,
    line: usize,
    interner: &'a mut Interner,
    pub output: Vec<Lexeme>,
}

impl<'a> Scanner<'a> {
    pub fn new(source_str: &'a str, interner: &'a mut Interner) -> Self {
        Self {
            source_str,
            selection_start: 0,
            selection_end: 0,
            line: 1,
            interner,
            output: Vec::new(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.selection_end >= self.source_str.len()
    }

    pub fn scan(&mut self) -> Result<()> {
        while !self.is_at_end() {
            self.selection_start = self.selection_end;
            self.scan_token()?
        }

        self.output.push(Lexeme {
            ty: Token::EOF,
            line_n: self.line as u32,
            start: self.source_str.len() as u32,
            len: 1,
            symbol: None,
        });

        Ok(())
    }

    fn scan_token(&mut self) -> Result<()> {
        let next = self.advance();
        match next {
            b' ' | b'\r' | b'\t' => (),
            b'(' => self.add_token(Token::LEFT_PAREN),
            b')' => self.add_token(Token::RIGHT_PAREN),
            b'{' => self.add_token(Token::LEFT_BRACE),
            b'}' => self.add_token(Token::RIGHT_BRACE),
            b',' => self.add_token(Token::COMMA),
            b'.' => self.add_token(Token::DOT),
            b'-' => self.add_token(Token::MINUS),
            b'+' => self.add_token(Token::PLUS),
            b';' => self.add_token(Token::SEMICOLON),
            b'*' => self.add_token(Token::STAR),

            b'!' if self.match_next(b'=') => self.add_token(Token::BANG_EQUAL),
            b'!' => self.add_token(Token::BANG),
            b'=' if self.match_next(b'=') => self.add_token(Token::EQUAL_EQUAL),
            b'=' => self.add_token(Token::EQUAL),
            b'>' if self.match_next(b'=') => self.add_token(Token::GREATER_EQUAL),
            b'>' => self.add_token(Token::GREATER),
            b'<' if self.match_next(b'=') => self.add_token(Token::LESS_EQUAL),
            b'<' => self.add_token(Token::LESS),

            b'/' if self.match_next(b'/') => {
                while self.peek() != b'\n' && !self.is_at_end() {
                    self.advance();
                }
            }
            b'/' if self.match_next(b'*') => {
                self.advance();
                while !(self.is_at_end() || self.peek() == b'*' && self.peek_next() == b'/') {
                    if self.peek() == b'\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                if !self.is_at_end() {
                    self.advance();
                    self.advance();
                } // skip '*/'
            }
            b'/' => self.add_token(Token::SLASH),

            b'"' => {
                while self.peek() != b'"' && !self.is_at_end() {
                    if self.peek() == b'\n' {
                        self.line += 1;
                    }
                    self.advance();
                }

                if self.is_at_end() {
                    return Err(CompileTimeError {
                        source_code: self.source_str.into(),
                        err_span: (self.selection_start..self.selection_start).into(),
                        advice: "an unterminated string was found!".into(),
                    }
                    .into());
                }

                self.advance();
                self.output.push(Lexeme::new(
                    Token::STRING,
                    self.line,
                    self.selection_start + 1,
                    self.selection_end - 1,
                    Some(
                        self.interner.intern(
                            self.source_str[self.selection_start + 1..self.selection_end - 1]
                                .to_owned(),
                        ),
                    ),
                ));
            }
            b'0'..=b'9' => {
                while self.peek().is_ascii_digit() {
                    self.advance();
                }
                if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
                    self.advance();
                    while self.peek().is_ascii_digit() {
                        self.advance();
                    }
                }
                self.add_token(Token::NUMBER);
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                while matches!(self.peek(), b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9') {
                    self.advance();
                }

                match &self.source_str.as_bytes()[self.selection_start..self.selection_end] {
                    b"and" => self.add_token(Token::AND),
                    b"class" => self.add_token(Token::CLASS),
                    b"else" => self.add_token(Token::ELSE),
                    b"false" => self.add_token(Token::FALSE),
                    b"for" => self.add_token(Token::FOR),
                    b"fun" => self.add_token(Token::FUN),
                    b"if" => self.add_token(Token::IF),
                    b"null" => self.add_token(Token::NULL),
                    b"or" => self.add_token(Token::OR),
                    b"print" => self.add_token(Token::PRINT),
                    b"return" => self.add_token(Token::RETURN),
                    b"super" => self.add_token(Token::SUPER),
                    b"this" => self.add_token(Token::THIS),
                    b"true" => self.add_token(Token::TRUE),
                    b"var" => self.add_token(Token::VAR),
                    b"while" => self.add_token(Token::WHILE),
                    _ => self.add_token_sy(Token::IDENTIFIER),
                }
            }
            b'\n' => self.line += 1,
            c => {
                return Err(CompileTimeError {
                    advice: format!(
                        "unknown character found: \'{}\'",
                        char::from_u32(c as u32).unwrap_or('§')
                    ),
                    source_code: self.source_str.into(),
                    err_span: (self.selection_start..self.selection_end).into(),
                }
                .into());
            }
        }
        Ok(())
    }

    fn match_next(&mut self, expected: u8) -> bool {
        if self.is_at_end() || self.source_str.as_bytes()[self.selection_end] != expected {
            return false;
        }
        self.selection_end += 1;
        true
    }

    fn advance(&mut self) -> u8 {
        let curr = self.source_str.as_bytes()[self.selection_end];
        self.selection_end += 1;
        curr
    }

    fn peek(&self) -> u8 {
        if self.is_at_end() {
            b'\0'
        } else {
            self.source_str.as_bytes()[self.selection_end]
        }
    }

    fn peek_next(&self) -> u8 {
        if (self.selection_end + 1) >= self.source_str.len() {
            return b'\0';
        }
        self.source_str.as_bytes()[self.selection_end + 1]
    }

    fn add_token(&mut self, ty: Token) {
        self.output.push(Lexeme::new(
            ty,
            self.line,
            self.selection_start,
            self.selection_end,
            None,
        ));
    }

    fn add_token_sy(&mut self, ty: Token) {
        self.output.push(Lexeme::new(
            ty,
            self.line,
            self.selection_start,
            self.selection_end,
            Some(
                self.interner.intern(
                    self.source_str[(self.selection_start)..(self.selection_end)].to_owned(),
                ),
            ),
        ));
    }
}
