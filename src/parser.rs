// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" | "%" ) unary )* ;
// unary          → ( "!" | "-" ) unary | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "none" | "(" expression ")" ;

use crate::expr::{Expr, Value};
use crate::token::{Token, TokenType};
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
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Expr {
        let mut parser = Parser::new(tokens);
        parser.expression()
    }

    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
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
            False => Expr::Literal(Value::False),
            True => Expr::Literal(Value::True),
            None => Expr::Literal(Value::None),
            Number | String => Expr::Literal(Value::Literal(token.literal.unwrap())),
            LeftParen => {
                let expression = Box::new(self.expression());
                if !self.matches(&[TokenType::RightParen]) {
                    panic!("Expected ')' after expression");
                }
                self.advance();
                Expr::Grouping { expression }
            }
            _ => panic!("Expected expression"),
        }
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
}
