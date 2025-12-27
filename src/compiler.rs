use miette::Result;

use crate::{
    errors::{ExpectExpression, ParseNumberError, TokenNotFound},
    interner::Interner,
    scanner::{Lexeme, Token},
    vm::{OpCode, VM, Value},
};

#[allow(non_camel_case_types)]
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
    fn next_higher(&self) -> Self {
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

#[derive(Debug, Clone, Copy)]
enum ParseFn {
    None,
    Grouping,
    Unary,
    Binary,
    Number,
    String,
    Literal,
}

// (prefix, infix, precedence)
type ParseRule = (ParseFn, ParseFn, Precedence);

fn parse_token_to_rule(token: &Token) -> ParseRule {
    use self::{ParseFn::*, Precedence::*, Token::*};

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
        _ => (None, None, PREC_NONE),
    }
}

pub struct Compiler<'a> {
    source: &'a str,
    input: &'a [Lexeme],
    vm: &'a mut VM,
    interner: &'a mut Interner,
    current: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(
        input: &'a [Lexeme],
        vm: &'a mut VM,
        interner: &'a mut Interner,
        source: &'a str,
    ) -> Self {
        Self {
            source,
            input,
            vm,
            interner,
            current: 0,
        }
    }

    fn parsefn_tofn(&mut self, rule: ParseFn) -> Result<()> {
        match rule {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Literal => self.literal(),
            ParseFn::String => self.string(),
            ParseFn::None => unreachable!(),
        }
    }

    pub fn compile(&mut self) -> Result<()> {
        self.expression()?;

        self.consume(Token::EOF)?;
        self.emit_instruction(OpCode::OP_RETURN);
        Ok(())
    }

    fn advance(&mut self) -> Result<()> {
        self.current += 1;
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::PREC_ASSIGNMENT)
    }

    fn number(&mut self) -> Result<()> {
        let Lexeme {
            ty: Token::NUMBER,
            line_n: _,
            start,
            end,
        } = &self.input[self.current - 1]
        else {
            unreachable!()
        };

        let bytes = &self.source.as_bytes()[*start..*end];
        let value = lexical_core::parse::<f64>(bytes).map_err(|_| ParseNumberError {
            source_code: self.source.into(),
            err_span: (*start..*end).into(),
        })?;

        self.emit_value(Value::Number(value));

        Ok(())
    }

    fn string(&mut self) -> Result<()> {
        let Lexeme {
            ty: Token::STRING,
            line_n: _,
            start,
            end,
        } = &self.input[self.current - 1]
        else {
            unreachable!()
        };

        let value = &self.source[*start..*end];
        let value = self.interner.intern(value.to_owned());
        self.emit_value(Value::Symbol(value));

        Ok(())
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(Token::RIGHT_PAREN)
    }

    fn unary(&mut self) -> Result<()> {
        let op_ty = &self.input[self.current - 1].ty;
        self.parse_precedence(Precedence::PREC_UNARY)?;

        match op_ty {
            Token::MINUS => self.emit_instruction(OpCode::OP_NEGATE),
            Token::BANG => self.emit_instruction(OpCode::OP_NOT),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn binary(&mut self) -> Result<()> {
        let op_ty = &self.input[self.current - 1].ty;
        let rule = parse_token_to_rule(op_ty);
        self.parse_precedence(rule.2.next_higher())?;

        macro_rules! emit_binop {
            ($self:expr, $op:expr, {$($token:ident => $opcode:ident),+ $(,)?}) => {
            match $op {
                $($token => $self.emit_instruction($opcode),)+
                _ => unreachable!(),}
            };
        }

        use self::{OpCode::*, Token::*};
        emit_binop!(self, op_ty, {
            PLUS => OP_ADD,
            MINUS => OP_SUBTRACT,
            STAR => OP_MULTIPLY,
            SLASH => OP_DIVIDE,
            EQUAL_EQUAL => OP_EQUAL,
            BANG_EQUAL => OP_NOT_EQUAL,
            GREATER => OP_GREATER,
            LESS => OP_LESS,
            GREATER_EQUAL => OP_GREATER_EQUAL,
            LESS_EQUAL => OP_LESS_EQUAL,
        });

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance()?;
        let last = &self.input[self.current - 1];
        let prefix_rule = parse_token_to_rule(&last.ty).0;
        if matches!(prefix_rule, ParseFn::None) {
            return Err(ExpectExpression {
                source_code: self.source.into(),
                err_span: (last.start..last.end).into(),
            }
            .into());
        };

        self.parsefn_tofn(prefix_rule)?;
        let v = precedence as u8;

        while v <= parse_token_to_rule(&self.input[self.current].ty).2 as u8 {
            self.advance()?;
            let last = &self.input[self.current - 1];
            let infix_rule = parse_token_to_rule(&last.ty).1;
            if matches!(infix_rule, ParseFn::None) {
                return Err(ExpectExpression {
                    source_code: self.source.into(),
                    err_span: (last.start..last.end).into(),
                }
                .into());
            };
            self.parsefn_tofn(infix_rule)?;
        }

        Ok(())
    }

    fn literal(&mut self) -> Result<()> {
        let last = &self.input[self.current - 1].ty;
        match last {
            Token::FALSE => self.emit_instruction(OpCode::OP_FALSE),
            Token::TRUE => self.emit_instruction(OpCode::OP_TRUE),
            Token::NULL => self.emit_instruction(OpCode::OP_NULL),
            _ => unreachable!(),
        }

        Ok(())
    }
    fn consume(&mut self, token: Token) -> Result<()> {
        let curr = &self.input[self.current];
        if curr.ty == token {
            return self.advance();
        }
        Err(TokenNotFound {
            advice: format!("A token of type '{:?}' was not found!", token),
            source_code: self.source.into(),
            err_span: (curr.start..curr.end).into(),
        }
        .into())
    }

    fn emit_instruction(&mut self, instruction: OpCode) {
        self.vm
            .add_instruction(instruction, self.input[self.current - 1].line_n);
    }

    fn emit_value(&mut self, value: Value) {
        let id = self.vm.add_value(value);
        self.emit_instruction(OpCode::OP_CONSTANT(id));
    }
}
