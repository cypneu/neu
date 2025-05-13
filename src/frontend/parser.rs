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
        let result = match self.peek().kind {
            TokenType::LeftBrace => self.block(),
            TokenType::If => self.if_statement(),
            TokenType::While => self.while_statement(),
            _ => self.expression_statement(),
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

    fn block(&mut self) -> ParserResult<Stmt> {
        self.advance();
        let mut statements = Vec::new();

        while !self.matches(&[TokenType::RightBrace, TokenType::Eof]) {
            if let Ok(stmt) = self.statement() {
                statements.push(stmt);
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block")?;
        Ok(Stmt::Block(statements))
    }

    fn if_statement(&mut self) -> ParserResult<Stmt> {
        self.advance();
        let condition = self.or()?;
        let then_branch = self.block()?;
        let else_branch = self.else_branch()?;
        Ok(Stmt::if_stmt(condition, then_branch, else_branch))
    }

    fn else_branch(&mut self) -> ParserResult<Option<Stmt>> {
        if !self.matches(&[TokenType::Else]) {
            return Ok(None);
        }

        self.advance();

        let branch = if self.peek().kind == TokenType::If {
            self.if_statement()?
        } else {
            self.block()?
        };
        Ok(Some(branch))
    }

    fn while_statement(&mut self) -> ParserResult<Stmt> {
        self.advance();
        let condition = self.or()?;
        let body = self.block()?;
        Ok(Stmt::while_stmt(condition, body))
    }

    fn expression(&mut self) -> ParserResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParserResult<Expr> {
        let lhs = self.or()?;
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

    fn or(&mut self) -> ParserResult<Expr> {
        let mut expr = self.and()?;

        while self.matches(&[TokenType::Or]) {
            let op = self.advance();
            let right = self.and()?;
            expr = Expr::logical(expr, op, right);
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParserResult<Expr> {
        let mut expr = self.equality()?;

        while self.matches(&[TokenType::And]) {
            let op = self.advance();
            let right = self.equality()?;
            expr = Expr::logical(expr, op, right)
        }

        Ok(expr)
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
        self.tokens.next().expect("scanner guaranteed EOF")
    }

    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn new(tokens: Vec<Token>, neu: &'a mut Neu) -> Self {
        let tokens = tokens.into_iter().peekable();
        Parser { tokens, neu }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::scanner::Scanner;

    fn parse_stmts(src: &str) -> Vec<Stmt> {
        let mut neu = Neu::new();
        let toks = Scanner::scan(src, &mut neu);
        Parser::parse(toks, &mut neu)
    }

    fn parse_expr(src: &str) -> Expr {
        let mut stmts = parse_stmts(src);
        assert_eq!(stmts.len(), 1, "expected exactly one statement");
        if let Stmt::Expr(expr) = stmts.remove(0) {
            expr
        } else {
            panic!("expected an expression statement");
        }
    }

    #[test]
    fn precedence_term_vs_factor() {
        let expr = parse_expr("1 + 2 * 3;");
        match expr {
            Expr::Binary { op, .. } => assert_eq!(op.kind, TokenType::Plus),
            _ => panic!("expected binary +"),
        }
    }

    #[test]
    fn parses_grouping() {
        let expr = parse_expr("(1 + 2) * 3;");
        match expr {
            Expr::Binary { op, .. } => assert_eq!(op.kind, TokenType::Star),
            _ => panic!("expected binary *"),
        }
    }

    #[test]
    fn parses_logical_and() {
        let expr = parse_expr("true and false;");
        match expr {
            Expr::Logical { op, .. } => assert_eq!(op.kind, TokenType::And),
            _ => panic!("expected logical and"),
        }
    }

    #[test]
    fn parses_logical_or() {
        let expr = parse_expr("true or false;");
        match expr {
            Expr::Logical { op, .. } => assert_eq!(op.kind, TokenType::Or),
            _ => panic!("expected logical or"),
        }
    }

    #[test]
    fn logical_precedence_and_over_or() {
        let expr = parse_expr("false or true and false;");
        match expr {
            Expr::Logical { op, right, .. } => {
                assert_eq!(op.kind, TokenType::Or);
                assert!(matches!(*right, Expr::Logical { op, .. } if op.kind == TokenType::And));
            }
            _ => panic!("expected logical or"),
        }
    }

    #[test]
    fn parses_simple_if() {
        let src = "if true { 1; }";
        let stmts = parse_stmts(src);
        assert_eq!(stmts.len(), 1);

        match &stmts[0] {
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if let Expr::Literal(Literal::Boolean(b)) = condition {
                    assert!(*b);
                } else {
                    panic!("expected Boolean literal in if condition");
                }

                if let Stmt::Block(inner) = &**then_branch {
                    assert_eq!(inner.len(), 1);
                } else {
                    panic!("expected then-branch to be a block");
                }

                assert!(else_branch.is_none());
            }
            _ => panic!("expected an if statement"),
        }
    }

    #[test]
    fn parses_if_else_if_else() {
        let src = "if false { 1; } else if true { 2; } else { 3; }";
        let stmts = parse_stmts(src);
        assert_eq!(stmts.len(), 1);

        match &stmts[0] {
            Stmt::If {
                else_branch: Some(else1),
                ..
            } => match &**else1 {
                Stmt::If {
                    condition,
                    else_branch,
                    ..
                } => {
                    if let Expr::Literal(Literal::Boolean(b)) = condition {
                        assert!(*b);
                    } else {
                        panic!("expected Boolean literal in else-if condition");
                    }
                    assert!(else_branch.is_some());
                }
                _ => panic!("expected nested if in else-branch"),
            },
            _ => panic!("expected if with else branch"),
        }
    }

    #[test]
    fn parses_while_statement() {
        let src = "while x < 5 { x = x + 1; }";
        let stmts = parse_stmts(src);
        assert_eq!(stmts.len(), 1);

        match &stmts[0] {
            Stmt::While { condition, body } => {
                match condition {
                    Expr::Binary { op, left, right } => {
                        assert_eq!(op.kind, TokenType::Less);
                        match &**left {
                            Expr::Variable { name } => assert_eq!(name.lexeme, "x"),
                            _ => panic!("Expected variable 'x' on left of <"),
                        }
                        match &**right {
                            Expr::Literal(Literal::Number(n)) => assert_eq!(*n, 5.0),
                            _ => panic!("Expected number literal 5 on right of <"),
                        }
                    }
                    _ => panic!("Expected binary expression for while condition"),
                }

                match &**body {
                    Stmt::Block(block_stmts) => {
                        assert_eq!(block_stmts.len(), 1);
                        match &block_stmts[0] {
                            Stmt::Expr(Expr::Assignment { name, value: _ }) => {
                                assert_eq!(name.lexeme, "x");
                            }
                            _ => panic!("Expected assignment expression in while body block"),
                        }
                    }
                    _ => panic!("Expected block statement for while body"),
                }
            }
            _ => panic!("Expected a while statement"),
        }
    }
}
