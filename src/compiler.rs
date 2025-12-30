use std::num::{NonZero, NonZeroUsize};

use miette::Result;

use crate::{
    errors::CompileTimeError,
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
    Variable,
    And,
    Or,
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
        IDENTIFIER => (Variable, None, PREC_NONE),
        AND => (None, And, PREC_AND),
        OR => (None, Or, PREC_OR),
        _ => (None, None, PREC_NONE),
    }
}

#[derive(Debug)]
struct Local {
    name: u32,
    depth: Option<NonZeroUsize>,
}

pub struct Compiler<'a> {
    source: &'a str,
    input: &'a [Lexeme],
    vm: &'a mut VM,
    current: usize,

    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(input: &'a [Lexeme], vm: &'a mut VM, source: &'a str) -> Self {
        Self {
            source,
            input,
            vm,
            current: 0,
            locals: Vec::new(),
            local_count: 0,
            scope_depth: 0,
        }
    }

    fn parsefn_tofn(&mut self, rule: ParseFn, can_assign: bool) -> Result<()> {
        match rule {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Literal => self.literal(),
            ParseFn::String => self.string(),
            ParseFn::Variable => self.variable(can_assign),
            ParseFn::None => unreachable!(),
            ParseFn::And => self.and(),
            ParseFn::Or => self.or(),
        }
    }

    pub fn compile(&mut self) -> Result<()> {
        while !self.check(Token::EOF) {
            self.declaration()?;
        }

        self.consume(Token::EOF)?;
        Ok(())
    }

    fn advance(&mut self) -> Result<()> {
        self.current += 1;
        Ok(())
    }

    fn declaration(&mut self) -> Result<()> {
        match &self.input[self.current].ty {
            Token::VAR => {
                self.advance()?;
                self.var_stmt()
            }
            _ => self.statement(),
        }
    }

    fn statement(&mut self) -> Result<()> {
        let curr = &self.input[self.current];

        match curr.ty {
            Token::PRINT => {
                self.advance()?;
                self.print_stmt()
            }
            Token::IF => {
                self.advance()?;
                self.if_stmt()
            }
            Token::WHILE => {
                self.advance()?;
                self.while_stmt()
            }
            Token::FOR => {
                self.advance()?;
                self.for_stmt()
            }
            Token::LEFT_BRACE => {
                self.advance()?;
                self.begin_scope();
                self.block()?;
                self.end_scope();
                Ok(())
            }
            _ => self.expression_stmt(),
        }
    }

    fn block(&mut self) -> Result<()> {
        while !self.check(Token::RIGHT_BRACE) && !self.check(Token::EOF) {
            self.declaration()?;
        }
        self.consume(Token::RIGHT_BRACE)?;
        Ok(())
    }

    fn print_stmt(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(Token::SEMICOLON)?;
        self.emit_instruction(OpCode::OP_PRINT);
        Ok(())
    }

    fn if_stmt(&mut self) -> Result<()> {
        self.consume(Token::LEFT_PAREN)?;
        self.expression()?;
        self.consume(Token::RIGHT_PAREN)?;

        let then_jump = self.emit_jump(OpCode::OP_JUMP_IF_FALSE(0));
        self.emit_instruction(OpCode::OP_POP);
        self.statement()?;

        let else_jump = self.emit_jump(OpCode::OP_JUMP(0));

        self.patch_jump(then_jump);
        self.emit_instruction(OpCode::OP_POP);
        if self.match_advance(Token::ELSE)? {
            self.statement()?;
        }
        self.patch_jump(else_jump);

        Ok(())
    }

    fn while_stmt(&mut self) -> Result<()> {
        let loop_start = self.vm.instructions.len();
        self.consume(Token::LEFT_PAREN)?;
        self.expression()?;
        self.consume(Token::RIGHT_PAREN)?;
        let exit_jump = self.emit_jump(OpCode::OP_JUMP_IF_FALSE(0));
        self.emit_instruction(OpCode::OP_POP);
        self.statement()?;
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
        self.emit_instruction(OpCode::OP_POP);
        Ok(())
    }

    fn for_stmt(&mut self) -> Result<()> {
        self.begin_scope();
        self.consume(Token::LEFT_PAREN)?;
        if self.match_advance(Token::SEMICOLON)? {
        } else if self.match_advance(Token::VAR)? {
            self.var_stmt()?;
        } else {
            self.expression_stmt()?;
        }
        let mut loop_start = self.vm.instructions.len();

        let mut exit_jump = None;
        if !self.match_advance(Token::SEMICOLON)? {
            self.expression()?;
            self.consume(Token::SEMICOLON)?;

            exit_jump = Some(self.emit_jump(OpCode::OP_JUMP_IF_FALSE(0)));
            self.emit_instruction(OpCode::OP_POP);
        }

        if !self.match_advance(Token::RIGHT_PAREN)? {
            let body_jump = self.emit_jump(OpCode::OP_JUMP(0));
            let inc_start = self.vm.instructions.len();
            self.expression()?;
            self.emit_instruction(OpCode::OP_POP);
            self.consume(Token::RIGHT_PAREN)?;
            self.emit_loop(loop_start);
            loop_start = inc_start;
            self.patch_jump(body_jump);
        }
        self.statement()?;
        self.emit_loop(loop_start);
        if let Some(v) = exit_jump {
            self.patch_jump(v);
            self.emit_instruction(OpCode::OP_POP);
        }
        self.end_scope();

        Ok(())
    }

    fn expression_stmt(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(Token::SEMICOLON)?;
        self.emit_instruction(OpCode::OP_POP);
        Ok(())
    }

    fn var_stmt(&mut self) -> Result<()> {
        let global = self.parse_variable_name()?;

        let curr = &self.input[self.current];

        match curr.ty {
            Token::EQUAL => {
                self.advance()?;
                self.expression()?
            }
            _ => self.emit_instruction(OpCode::OP_NULL),
        }
        self.consume(Token::SEMICOLON)?;
        match self.scope_depth > 0 {
            true => {
                self.mark_initialised();
            }
            false => self.emit_instruction(OpCode::OP_DEFINE_GLOBAL(global)),
        }
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::PREC_ASSIGNMENT)
    }

    fn number(&mut self) -> Result<()> {
        match &self.input[self.current - 1] {
            x @ Lexeme {
                ty: Token::NUMBER, ..
            } => {
                let bytes = x.find(self.source).as_bytes();
                let value = lexical_core::parse::<f64>(bytes).map_err(|_| CompileTimeError {
                    source_code: self.source.into(),
                    err_span: x.into(),
                    advice: format!(
                        "could not parse \"{}\" as number!",
                        str::from_utf8(bytes).unwrap()
                    ),
                })?;

                self.emit_value(Value::Number(value));
            }
            _ => unreachable!(),
        };

        // let bytes =

        Ok(())
    }

    fn string(&mut self) -> Result<()> {
        let Lexeme {
            ty: Token::STRING,
            symbol,
            ..
        } = &self.input[self.current - 1]
        else {
            unreachable!()
        };
        self.emit_value(Value::Symbol(symbol.unwrap().get()));

        Ok(())
    }

    fn grouping(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(Token::RIGHT_PAREN)?;
        Ok(())
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

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let name = self.get_last_as_interned();

        let arg = self.resolve_local(name)?;
        let (get_op, set_op);
        match arg {
            None => {
                get_op = OpCode::OP_GET_GLOBAL(name);
                set_op = OpCode::OP_SET_GLOBAL(name)
            }
            Some(u) => {
                get_op = OpCode::OP_GET_LOCAL(u);
                set_op = OpCode::OP_SET_LOCAL(u)
            }
        };

        match &self.input[self.current].ty {
            Token::EQUAL if can_assign => {
                self.advance()?;
                self.expression()?;
                self.emit_instruction(set_op);
            }
            _ => {
                self.emit_instruction(get_op);
            }
        }

        Ok(())
    }

    fn and(&mut self) -> Result<()> {
        let end_jump = self.emit_jump(OpCode::OP_JUMP_IF_FALSE(0));
        self.emit_instruction(OpCode::OP_POP);
        self.parse_precedence(Precedence::PREC_AND)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn or(&mut self) -> Result<()> {
        let else_jump = self.emit_jump(OpCode::OP_JUMP_IF_FALSE(0));
        let end_jump = self.emit_jump(OpCode::OP_JUMP(0));

        self.patch_jump(else_jump);
        self.emit_instruction(OpCode::OP_POP);
        self.parse_precedence(Precedence::PREC_OR)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn resolve_local(&mut self, name: u32) -> Result<Option<u32>> {
        for i in (0..self.local_count).rev() {
            let local = &self.locals[i];
            if name == local.name {
                if local.depth.is_none() {
                    let last = &self.input[self.current - 1];
                    return Err(CompileTimeError {
                        advice: "can't read local variable in its own initialiser!".into(),
                        source_code: self.source.into(),
                        err_span: last.into(),
                    }
                    .into());
                }
                return Ok(Some(i as u32));
            }
        }
        Ok(None)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        self.advance()?;
        let last = &self.input[self.current - 1];
        let prefix_rule = parse_token_to_rule(&last.ty).0;
        if matches!(prefix_rule, ParseFn::None) {
            return Err(CompileTimeError {
                source_code: self.source.into(),
                err_span: last.into(),
                advice: "expected an expression!".into(),
            }
            .into());
        };

        let v = precedence as u8;
        let can_assign = v <= Precedence::PREC_ASSIGNMENT as u8;

        self.parsefn_tofn(prefix_rule, can_assign)?;

        while v <= parse_token_to_rule(&self.input[self.current].ty).2 as u8 {
            self.advance()?;
            let last = &self.input[self.current - 1];
            let infix_rule = parse_token_to_rule(&last.ty).1;
            if matches!(infix_rule, ParseFn::None) {
                return Err(CompileTimeError {
                    source_code: self.source.into(),
                    err_span: last.into(),
                    advice: "expected an expression!".into(),
                }
                .into());
            };
            self.parsefn_tofn(infix_rule, can_assign)?;

            if can_assign && self.check(Token::EQUAL) {
                return Err(CompileTimeError {
                    source_code: self.source.into(),
                    err_span: last.into(),
                    advice: "this is an invalid assignment target!".into(),
                }
                .into());
            }
        }

        Ok(())
    }

    fn parse_variable_name(&mut self) -> Result<u32> {
        self.consume(Token::IDENTIFIER)?;
        self.declare_variable()?;
        Ok(match self.scope_depth > 0 {
            true => 0,
            false => self.input[self.current - 1].symbol.unwrap().get(),
        })
    }

    fn mark_initialised(&mut self) {
        self.locals[self.local_count - 1].depth = NonZero::new(self.scope_depth)
    }

    fn declare_variable(&mut self) -> Result<()> {
        if self.scope_depth == 0 {
            return Ok(());
        }

        let name = self.get_last_as_interned();

        for local in self.locals.iter().rev() {
            match local.depth {
                Some(d) if d.get() < self.scope_depth => break,
                Some(_) | None => {
                    if name == local.name {
                        let last = &self.input[self.current - 1];
                        return Err(CompileTimeError {
                            advice: "there already exists a variable with this name in this scope."
                                .into(),
                            source_code: self.source.into(),
                            err_span: last.into(),
                        }
                        .into());
                    }
                }
            }
        }

        self.add_local(name);
        Ok(())
    }

    fn add_local(&mut self, name: u32) {
        self.locals.push(Local { name, depth: None });
        // local.depth = NonZero::new(self.scope_depth);
        self.local_count += 1;
    }

    fn check(&mut self, token: Token) -> bool {
        let curr = &self.input[self.current];
        if curr.ty == token {
            return true;
        }
        false
    }

    fn match_advance(&mut self, token: Token) -> Result<bool> {
        if self.check(token) {
            self.consume(token)?;
            return Ok(true);
        }
        Ok(false)
    }

    fn consume(&mut self, token: Token) -> Result<&Lexeme> {
        let curr = &self.input[self.current];
        if curr.ty == token {
            let _ = self.advance();
            return Ok(curr);
        }
        Err(CompileTimeError {
            advice: format!("A token of type '{:?}' was not found!", token),
            source_code: self.source.into(),
            err_span: curr.into(),
        }
        .into())
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.vm.add_value(value)
    }

    fn emit_instruction(&mut self, instruction: OpCode) {
        self.vm
            .add_instruction(instruction, self.input[self.current - 1].line_n as usize);
    }

    fn emit_value(&mut self, value: Value) {
        let v = self.add_constant(value);
        self.emit_instruction(OpCode::OP_CONSTANT(v as u32));
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        while self.local_count > 0 {
            let remove = match self.locals[self.local_count - 1].depth {
                Some(d) => d.get() > self.scope_depth,
                None => true,
            };

            if !remove {
                break;
            }

            self.emit_instruction(OpCode::OP_POP);
            self.local_count -= 1;
            self.locals.pop();
        }
    }

    fn get_last_as_interned(&mut self) -> u32 {
        self.input[self.current - 1].symbol.unwrap().get()
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_instruction(instruction);
        self.vm.instructions.len() - 1
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.vm.instructions.len() - 1 - offset;
        self.vm.instructions[offset] = match self.vm.instructions[offset] {
            OpCode::OP_JUMP_IF_FALSE(_) => OpCode::OP_JUMP_IF_FALSE(jump as u32),
            OpCode::OP_JUMP(_) => OpCode::OP_JUMP(jump as u32),
            _ => unreachable!(),
        }
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let off_set = self.vm.instructions.len() + 1 - loop_start;
        self.emit_instruction(OpCode::OP_LOOP(off_set as u32));
    }
}
