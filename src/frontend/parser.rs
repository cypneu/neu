use crate::ast::expr::Expr;
use crate::frontend::literal::Literal;
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

#[derive(Debug)]
pub struct ParseError {
    token: Token,
    message: String,
}

impl ParseError {
    fn new(token: &Token, message: &str) -> Self {
        Self {
            message: message.to_string(),
            token: token.clone(),
        }
    }
}

type ParserResult = Result<Expr, ParseError>;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token>>,
    neu: &'a mut Neu,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<Token>, neu: &'a mut Neu) -> Option<Expr> {
        let mut parser = Parser::new(tokens, neu);
        parser.expression().ok()
    }

    fn expression(&mut self) -> ParserResult {
        self.equality()
    }

    fn equality(&mut self) -> ParserResult {
        self.binary_expr(Self::comparison, &EQUALITY_OPERATORS)
    }

    fn comparison(&mut self) -> ParserResult {
        self.binary_expr(Self::term, &COMPARISON_OPERATORS)
    }

    fn term(&mut self) -> ParserResult {
        self.binary_expr(Self::factor, &TERM_OPERATORS)
    }

    fn factor(&mut self) -> ParserResult {
        self.binary_expr(Self::unary, &FACTOR_OPERATORS)
    }

    fn unary(&mut self) -> ParserResult {
        if self.matches(&UNARY_OPERATORS) {
            let operator = self.advance().unwrap();
            let right = Box::new(self.unary()?);
            return Ok(Expr::Unary { operator, right });
        }

        self.primary()
    }

    fn primary(&mut self) -> ParserResult {
        let token = self.advance().unwrap();
        use TokenType::*;
        match token.kind {
            False => Ok(Expr::Literal(Literal::from(false))),
            True => Ok(Expr::Literal(Literal::from(true))),
            None => Ok(Expr::Literal(Literal::None)),
            Number | String => Ok(Expr::Literal(token.literal.unwrap())),
            LeftParen => {
                let expression = Box::new(self.expression()?);
                if !self.matches(&[TokenType::RightParen]) {
                    return Err(self.error(&token, "Expected ')' after expression"));
                }
                self.advance();
                Ok(Expr::Grouping { expression })
            }
            _ => Err(self.error(&token, "Expected expression")),
        }
    }

    fn error(&mut self, token: &Token, message: &str) -> ParseError {
        let location = match token.kind {
            TokenType::Eof => " at end".into(),
            _ => format!(" at '{}'", token.lexeme),
        };
        self.neu.report(token.line, location, message.into());
        ParseError::new(token, message)
    }

    fn synchronize() {
        unimplemented!()
    }

    fn binary_expr<F>(&mut self, mut operand: F, operators: &[TokenType]) -> ParserResult
    where
        F: FnMut(&mut Self) -> ParserResult,
    {
        let mut expr = operand(self)?;

        while self.matches(operators) {
            let operator = self.advance().unwrap();
            let right = operand(self)?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn matches(&mut self, token_types: &[TokenType]) -> bool {
        self.peek().is_some_and(|token| {
            token_types
                .iter()
                .any(|token_type| *token_type == token.kind)
        })
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn new(tokens: Vec<Token>, neu: &'a mut Neu) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            neu,
        }
    }
}
