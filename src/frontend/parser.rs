use crate::ast::expr::Expr;
use crate::ast::stmt::Stmt;
use crate::frontend::literal::Literal;
use crate::frontend::parser_error::ParseError;
use crate::frontend::token::{Token, TokenType};
use crate::Neu;

use std::iter::Peekable;
use std::vec::IntoIter;

const EQUALITY_OPERATORS: [TokenType; 2] = [TokenType::EqualEqual, TokenType::BangEqual];
const COMPARISON_OPERATORS: [TokenType; 4] = [
    TokenType::Greater,
    TokenType::GreaterEqual,
    TokenType::Less,
    TokenType::LessEqual,
];
const TERM_OPERATORS: [TokenType; 2] = [TokenType::Minus, TokenType::Plus];
const FACTOR_OPERATORS: [TokenType; 3] = [TokenType::Slash, TokenType::Star, TokenType::Modulo];
const UNARY_OPERATORS: [TokenType; 2] = [TokenType::Bang, TokenType::Minus];

type ParserResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token>>,
    neu: &'a mut Neu,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<Token>, neu: &'a mut Neu) -> Vec<Stmt> {
        let mut parser = Parser::new(tokens, neu);

        let mut statements = Vec::new();
        while !parser.matches(&[TokenType::Eof]) {
            if let Ok(stmt) = parser.statement() {
                statements.push(stmt);
            }
        }

        statements
    }

    fn statement(&mut self) -> ParserResult<Stmt> {
        let result = if self.matches(&[TokenType::LeftBrace]) {
            Ok(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn expression_statement(&mut self) -> ParserResult<Stmt> {
        let expr = self.expression()?;

        let msg = "Expect ';' after expression statement.";
        self.consume(TokenType::Semicolon, msg)?;

        Ok(Stmt::Expr(expr))
    }

    fn block(&mut self) -> ParserResult<Vec<Stmt>> {
        self.advance();
        let mut statements = Vec::new();

        while !self.matches(&[TokenType::RightBrace, TokenType::Eof]) {
            if let Ok(stmt) = self.statement() {
                statements.push(stmt);
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block")?;
        Ok(statements)
    }

    fn expression(&mut self) -> ParserResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParserResult<Expr> {
        let lhs = self.equality()?;
        if !self.matches(&[TokenType::Equal]) {
            return Ok(lhs);
        }

        let equals = self.advance();
        let rhs = self.assignment()?;

        match lhs {
            Expr::Variable { name } => Ok(Expr::assign(name, rhs)),
            _ => Err(self.error(&equals, "Invalid target assignment")),
        }
    }

    fn equality(&mut self) -> ParserResult<Expr> {
        self.binary_expr(Self::comparison, &EQUALITY_OPERATORS)
    }

    fn comparison(&mut self) -> ParserResult<Expr> {
        self.binary_expr(Self::term, &COMPARISON_OPERATORS)
    }

    fn term(&mut self) -> ParserResult<Expr> {
        self.binary_expr(Self::factor, &TERM_OPERATORS)
    }

    fn factor(&mut self) -> ParserResult<Expr> {
        self.binary_expr(Self::unary, &FACTOR_OPERATORS)
    }

    fn unary(&mut self) -> ParserResult<Expr> {
        if self.matches(&UNARY_OPERATORS) {
            let operator = self.advance();
            let right = self.unary()?;
            return Ok(Expr::unary(operator, right));
        }

        self.primary()
    }

    fn primary(&mut self) -> ParserResult<Expr> {
        let token = self.advance();
        let expr = match token.kind {
            TokenType::False => false.into(),
            TokenType::True => true.into(),
            TokenType::None => Literal::None.into(),
            TokenType::Number | TokenType::String => token.literal.unwrap().into(),
            TokenType::LeftParen => {
                let expression = self.expression()?;
                self.consume(TokenType::RightParen, "Expected ')' after expression")?;
                Expr::group(expression)
            }
            TokenType::Identifier => Expr::variable(token),
            _ => return Err(self.error(&token, "Expected expression")),
        };
        Ok(expr)
    }

    fn error(&mut self, token: &Token, message: &str) -> ParseError {
        let location = match token.kind {
            TokenType::Eof => " at end".into(),
            _ => format!(" at '{}'", token.lexeme),
        };
        self.neu.report(token.line, location, message.into());
        ParseError::new(token, message)
    }

    fn synchronize(&mut self) {
        while !self.matches(&[TokenType::Eof]) {
            match self.peek().kind {
                TokenType::Semicolon => {
                    self.advance();
                    return;
                }
                TokenType::Class
                | TokenType::Fn
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Return => return,
                _ => self.advance(),
            };
        }
    }

    fn binary_expr<F>(&mut self, mut operand: F, operators: &[TokenType]) -> ParserResult<Expr>
    where
        F: FnMut(&mut Self) -> ParserResult<Expr>,
    {
        let mut expr = operand(self)?;

        while self.matches(operators) {
            let operator = self.advance();
            let right = operand(self)?;
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    fn consume(&mut self, kind: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.peek().kind == kind {
            Ok(self.advance())
        } else {
            let token = self.peek().clone();
            Err(self.error(&token, message))
        }
    }

    fn matches(&mut self, token_types: &[TokenType]) -> bool {
        token_types.contains(&self.peek().kind)
    }

    fn advance(&mut self) -> Token {
        self.tokens.next().unwrap()
    }

    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn new(tokens: Vec<Token>, neu: &'a mut Neu) -> Self {
        let tokens = tokens.into_iter().peekable();
        Parser { tokens, neu }
    }
}
