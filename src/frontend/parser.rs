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

struct ParseError;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token>>,
    neu: &'a mut Neu,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Vec<Token>, neu: &'a mut Neu) -> Expr {
        let mut parser = Parser::new(tokens, neu);
        parser.expression()
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        self.binary_expr(Self::comparison, &EQUALITY_OPERATORS)
    }

    fn comparison(&mut self) -> Expr {
        self.binary_expr(Self::term, &COMPARISON_OPERATORS)
    }

    fn term(&mut self) -> Expr {
        self.binary_expr(Self::factor, &TERM_OPERATORS)
    }

    fn factor(&mut self) -> Expr {
        self.binary_expr(Self::unary, &FACTOR_OPERATORS)
    }

    fn unary(&mut self) -> Expr {
        if self.matches(&UNARY_OPERATORS) {
            let operator = self.advance().unwrap();
            let right = Box::new(self.unary());
            return Expr::Unary { operator, right };
        }

        self.primary()
    }

    fn primary(&mut self) -> Expr {
        let token = self.advance().unwrap();
        use TokenType::*;
        match token.kind {
            False => Expr::Literal(Literal::from(false)),
            True => Expr::Literal(Literal::from(true)),
            None => Expr::Literal(Literal::None),
            Number | String => Expr::Literal(token.literal.unwrap()),
            LeftParen => {
                let expression = Box::new(self.expression());
                if !self.matches(&[TokenType::RightParen]) {
                    self.error(token, "Expected ')' after expression".into());
                    panic!("Expected ')' after expression");
                }
                self.advance();
                Expr::Grouping { expression }
            }
            _ => {
                self.error(token, "Expected expression".into());
                panic!("Expected expression")
            }
        }
    }

    fn error(&mut self, token: Token, message: String) -> ParseError {
        let location = match token.kind {
            TokenType::Eof => " at end".into(),
            _ => format!(" at '{}'", token.lexeme),
        };
        self.neu.report(token.line, location, message);
        ParseError
    }

    fn synchronize() {
        unimplemented!()
    }

    fn binary_expr<F>(&mut self, mut operand: F, operators: &[TokenType]) -> Expr
    where
        F: FnMut(&mut Self) -> Expr,
    {
        let mut expr = operand(self);

        while self.matches(operators) {
            let operator = self.advance().unwrap();
            let right = operand(self);
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        expr
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
