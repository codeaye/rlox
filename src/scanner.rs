use std::sync::Arc;

use miette::Result;

use crate::{
    errors::CompileTimeError,
    interner::Interner,
    typedef::{Lexeme, Token},
};

#[derive(Default, Debug)]
pub struct Cursor {
    start: usize,
    end: usize,
}

impl Cursor {
    pub(crate) fn advance(&mut self) {
        self.end += 1;
    }
    pub(crate) fn begin_next(&mut self) {
        self.start = self.end;
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a [u8],
    source_str: Arc<str>,
    cursor: Cursor,
    line: u32,

    interner: &'a mut Interner,

    done: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a [u8], source_str: Arc<str>, interner: &'a mut Interner) -> Self {
        Self {
            source,
            source_str,
            cursor: Cursor::default(),
            line: 1,
            interner,
            done: false,
        }
    }

    fn at_end(&self) -> bool {
        self.cursor.end >= self.source.len()
    }

    fn peek(&self) -> u8 {
        self.source.get(self.cursor.end).copied().unwrap_or(b'\0')
    }

    fn peek_next(&self) -> u8 {
        self.source
            .get(self.cursor.end + 1)
            .copied()
            .unwrap_or(b'\0')
    }

    fn advance(&mut self) -> u8 {
        let curr = self.source.get(self.cursor.end).copied().unwrap_or(b'\0');
        self.cursor.advance();
        curr
    }

    fn match_next(&mut self, expected: u8) -> bool {
        match self.peek() == expected {
            true => {
                self.cursor.advance();
                true
            }
            false => false,
        }
    }

    fn make_token(&self, ty: Token) -> Option<Result<Lexeme>> {
        Some(Ok(Lexeme::new(
            ty,
            self.line,
            self.cursor.start,
            self.cursor.end,
            None,
        )))
    }

    fn make_token_with_symbol(&mut self, ty: Token) -> Option<Result<Lexeme>> {
        Some(Ok(Lexeme::new(
            ty,
            self.line,
            self.cursor.start,
            self.cursor.end,
            Some(self.interner.intern(self.cursor.start..self.cursor.end)),
        )))
    }

    fn scan_token(&mut self) -> Result<Token> {
        let next = self.advance();
        Ok(match next {
            b' ' | b'\r' | b'\t' => Token::IGNORABLE,
            b'(' => Token::LEFT_PAREN,
            b')' => Token::RIGHT_PAREN,
            b'{' => Token::LEFT_BRACE,
            b'}' => Token::RIGHT_BRACE,
            b',' => Token::COMMA,
            b'.' => Token::DOT,
            b'-' => Token::MINUS,
            b'+' => Token::PLUS,
            b';' => Token::SEMICOLON,
            b'*' => Token::STAR,

            b'!' if self.match_next(b'=') => Token::BANG_EQUAL,
            b'!' => Token::BANG,
            b'=' if self.match_next(b'=') => Token::EQUAL_EQUAL,
            b'=' => Token::EQUAL,
            b'>' if self.match_next(b'=') => Token::GREATER_EQUAL,
            b'>' => Token::GREATER,
            b'<' if self.match_next(b'=') => Token::LESS_EQUAL,
            b'<' => Token::LESS,

            b'/' if self.match_next(b'/') => {
                while self.peek() != b'\n' && !self.at_end() {
                    self.advance();
                }
                Token::IGNORABLE
            }
            b'/' if self.match_next(b'*') => {
                self.advance();
                while !(self.at_end() || self.peek() == b'*' && self.peek_next() == b'/') {
                    if self.peek() == b'\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                if !self.at_end() {
                    self.advance();
                    self.advance();
                } // skip '*/'
                Token::IGNORABLE
            }
            b'/' => Token::SLASH,

            b'"' => {
                while self.peek() != b'"' && !self.at_end() {
                    if self.peek() == b'\n' {
                        self.line += 1;
                    }
                    self.advance();
                }

                if self.at_end() {
                    return Err(CompileTimeError {
                        source_code: self.source_str.clone(),
                        err_span: (self.cursor.start..self.cursor.start).into(),
                        advice: "an unterminated string was found!".into(),
                    }
                    .into());
                }

                self.advance();

                Token::STRING
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
                Token::NUMBER
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                while matches!(self.peek(), b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9') {
                    self.advance();
                }

                match &self.source[self.cursor.start..self.cursor.end] {
                    b"and" => Token::AND,
                    b"class" => Token::CLASS,
                    b"else" => Token::ELSE,
                    b"false" => Token::FALSE,
                    b"for" => Token::FOR,
                    b"fun" => Token::FUN,
                    b"if" => Token::IF,
                    b"null" => Token::NULL,
                    b"or" => Token::OR,
                    b"print" => Token::PRINT,
                    b"return" => Token::RETURN,
                    b"super" => Token::SUPER,
                    b"this" => Token::THIS,
                    b"true" => Token::TRUE,
                    b"var" => Token::VAR,
                    b"while" => Token::WHILE,
                    _ => Token::IDENTIFIER,
                }
            }
            b'\n' => {
                self.line += 1;
                Token::IGNORABLE
            }
            c => {
                return Err(CompileTimeError {
                    advice: format!(
                        "unknown character found: \'{}\'",
                        char::from_u32(c as u32).unwrap_or('§')
                    ),
                    source_code: self.source_str.clone(),
                    err_span: (self.cursor.start..self.cursor.end).into(),
                }
                .into());
            }
        })
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Lexeme>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        loop {
            if self.at_end() {
                self.done = true;
                return self.make_token(Token::EOF);
            }
            self.cursor.begin_next();
            match self.scan_token() {
                Ok(Token::IGNORABLE) => continue,
                Ok(v @ Token::IDENTIFIER) => {
                    return self.make_token_with_symbol(v);
                }
                Ok(v @ Token::STRING) => {
                    return Some(Ok(Lexeme::new(
                        v,
                        self.line,
                        self.cursor.start,
                        self.cursor.end,
                        Some(
                            self.interner
                                .intern(self.cursor.start + 1..self.cursor.end - 1),
                        ),
                    )));
                }
                Ok(v) => return self.make_token(v),
                Err(v) => return Some(Err(v)),
            }
        }
    }
}
