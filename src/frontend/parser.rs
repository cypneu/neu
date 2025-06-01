use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

use crate::ast::expr::Expr;
use crate::ast::stmt::{FunctionDecl, Stmt};
use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};
use crate::Neu;

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
    pub fn new(token: &Token, message: &str) -> Self {
        Self {
            message: message.to_string(),
            token: token.clone(),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token>>,
    neu: &'a mut Neu,
    function_nesting_depth: u32,
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

    fn statement(&mut self) -> ParseResult<Stmt> {
        let result = match self.peek().kind {
            TokenType::LeftBrace => self.block(),
            TokenType::If => self.if_statement(),
            TokenType::While => self.while_statement(),
            TokenType::For => self.for_statement(),
            TokenType::Struct => self.struct_declaration(),
            TokenType::Fn => self.function_declaration(),
            TokenType::Return => self.return_statement(),
            _ => self.expression_statement(),
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;

        let msg = "Expect ';' after expression statement.";
        self.consume(TokenType::Semicolon, msg)?;

        Ok(Stmt::Expr(expr))
    }

    fn block(&mut self) -> ParseResult<Stmt> {
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

    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let condition = self.or(false)?;
        let then_branch = self.block()?;
        let else_branch = self.else_branch()?;
        Ok(Stmt::if_stmt(condition, then_branch, else_branch))
    }

    fn else_branch(&mut self) -> ParseResult<Option<Stmt>> {
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

    fn while_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let condition = self.or(false)?;
        let body = self.block()?;
        Ok(Stmt::while_stmt(condition, body))
    }

    fn for_statement(&mut self) -> ParseResult<Stmt> {
        self.advance();

        let token = self.peek().clone();
        let var = self.primary()?;
        let name = match &var {
            Expr::Variable { name } => name,
            _ => return Err(self.error(&token, "Expected loop variable")),
        };

        self.consume(TokenType::In, "Expected 'in' in for loop")?;
        let start = self.unary(false)?;
        self.consume(TokenType::DotDot, "Expected '..' in range for loop")?;
        let end = self.unary(false)?;

        let mut body = self.block()?;

        let init = Stmt::Expr(Expr::assign(name.clone(), start));

        let lt_token = Token::new(TokenType::Less, "<".into(), None, name.line);
        let cond = Expr::binary(var.clone(), lt_token, end);

        let plus_token = Token::new(TokenType::Plus, "+".into(), None, name.line);
        let one = Expr::Literal(Literal::Number(1.0));
        let inc = Stmt::Expr(Expr::assign(
            name.clone(),
            Expr::binary(var, plus_token, one),
        ));

        if let Stmt::Block(stmts) = &mut body {
            stmts.push(inc);
        }

        Ok(Stmt::Block(vec![init, Stmt::while_stmt(cond, body)]))
    }

    fn struct_declaration(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let name = self.consume(TokenType::Identifier, "Expected struct name")?;

        self.consume(TokenType::LeftBrace, "Expected '{' before struct body")?;

        let mut fields = Vec::new();
        let mut last_was_comma = false;
        while self.matches(&[TokenType::Identifier]) {
            fields.push(self.advance());
            last_was_comma = false;

            if self.matches(&[TokenType::Comma]) {
                self.advance();
                last_was_comma = true;
                continue;
            }
            break;
        }

        if !fields.is_empty() && self.matches(&[TokenType::Fn]) && !last_was_comma {
            let token = self.peek().clone();
            return Err(self.error(
                &token.clone(),
                "Expected ',' after last field before method definitions",
            ));
        }

        let mut methods = Vec::new();
        while !self.matches(&[TokenType::RightBrace, TokenType::Eof]) {
            let method_decl = self.parse_callable_declaration("method")?;
            methods.push(Rc::new(method_decl));
        }

        self.consume(TokenType::RightBrace, "Expected '}' after struct body")?;

        Ok(Stmt::struct_declaration(name, fields, methods))
    }

    fn function_declaration(&mut self) -> ParseResult<Stmt> {
        let func_decl = self.parse_callable_declaration("function")?;
        Ok(Stmt::func_declaration(func_decl))
    }

    fn parse_callable_declaration(&mut self, kind: &str) -> Result<FunctionDecl, ParseError> {
        self.advance();

        let name = self.consume(TokenType::Identifier, &format!("Expected {} name.", kind))?;

        let params = self.parse_parameter_list(kind)?;

        let body = self.parse_callable_body()?;

        Ok(FunctionDecl { name, params, body })
    }

    fn parse_callable_body(&mut self) -> ParseResult<Vec<Stmt>> {
        self.function_nesting_depth += 1;
        let body = match self.block()? {
            Stmt::Block(stmts) => stmts,
            _ => unreachable!(),
        };
        self.function_nesting_depth -= 1;
        Ok(body)
    }

    fn parse_parameter_list(&mut self, kind: &str) -> ParseResult<Vec<Token>> {
        self.consume(
            TokenType::LeftParen,
            &format!("Expected '(' in {} declaration", kind),
        )?;

        let mut params = Vec::new();

        if !self.matches(&[TokenType::RightParen]) {
            loop {
                if params.len() >= 255 {
                    let token = self.peek().clone();
                    return Err(self.error(&token, "Can't have more than 255 parameters."));
                }

                let param = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                params.push(param);
                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected a comma after a parameter.")?;
            }
        }

        self.consume(
            TokenType::RightParen,
            &format!("Expected ')' in {} declaration", kind),
        )?;
        Ok(params)
    }

    fn return_statement(&mut self) -> ParseResult<Stmt> {
        let token = self.advance();
        if self.function_nesting_depth == 0 {
            return Err(self.error(&token, "Return can be used only within a function."));
        }

        let value = (!self.matches(&[TokenType::Semicolon]))
            .then(|| self.or(true))
            .transpose()?;

        self.consume(TokenType::Semicolon, "Expected ';' after return value.")?;
        Ok(Stmt::Return { value })
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let lhs = self.or(true)?;
        if !self.matches(&[TokenType::Equal]) {
            return Ok(lhs);
        }

        let equals = self.advance();
        let rhs = self.assignment()?;

        match lhs {
            Expr::Variable { name } => Ok(Expr::assign(name, rhs)),
            Expr::Get { name, expr } => Ok(Expr::set(expr, name, rhs)),
            _ => Err(self.error(&equals, "Invalid target assignment")),
        }
    }

    fn or(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        let mut expr = self.and(struct_ok)?;

        while self.matches(&[TokenType::Or]) {
            let op = self.advance();
            let right = self.and(struct_ok)?;
            expr = Expr::logical(expr, op, right);
        }

        Ok(expr)
    }

    fn and(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        let mut expr = self.equality(struct_ok)?;

        while self.matches(&[TokenType::And]) {
            let op = self.advance();
            let right = self.equality(struct_ok)?;
            expr = Expr::logical(expr, op, right)
        }

        Ok(expr)
    }

    fn equality(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::comparison, struct_ok, &EQUALITY_OPERATORS)
    }

    fn comparison(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::term, struct_ok, &COMPARISON_OPERATORS)
    }

    fn term(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::factor, struct_ok, &TERM_OPERATORS)
    }

    fn factor(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::unary, struct_ok, &FACTOR_OPERATORS)
    }

    fn unary(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        if self.matches(&UNARY_OPERATORS) {
            let operator = self.advance();
            let right = self.unary(struct_ok)?;
            return Ok(Expr::unary(operator, right));
        }

        self.call(struct_ok)
    }

    fn call(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.matches(&[TokenType::Dot]) {
                self.advance();
                let name =
                    self.consume(TokenType::Identifier, "Expect property name after '.'.")?;
                expr = Expr::get(expr, name);
            } else if struct_ok && self.matches(&[TokenType::LeftBrace]) {
                self.advance();

                let mut fields = Vec::new();
                if !self.matches(&[TokenType::RightBrace]) {
                    loop {
                        let name = self.consume(
                            TokenType::Identifier,
                            "Expected field name in struct literal",
                        )?;
                        self.consume(TokenType::Colon, "Expected ':' after field name")?;

                        let value = self.expression()?;
                        fields.push((name, value));

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                        self.advance();
                    }
                }

                self.consume(TokenType::RightBrace, "Expected '}' after struct literal")?;
                expr = Expr::struct_init(expr, fields);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        self.advance();

        let mut arguments = Vec::new();
        if !self.matches(&[TokenType::RightParen]) {
            loop {
                if arguments.len() >= 255 {
                    let token = self.peek().clone();
                    return Err(self.error(&token, "Can't have more than 255 arguments."));
                }

                arguments.push(self.or(true)?);
                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected a comma after an argument.")?;
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(Expr::call(callee, arguments))
    }

    fn primary(&mut self) -> ParseResult<Expr> {
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
                TokenType::Struct
                | TokenType::Fn
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Return => return,
                _ => self.advance(),
            };
        }
    }

    fn binary_expr<F>(
        &mut self,
        mut operand: F,
        struct_ok: bool,
        operators: &[TokenType],
    ) -> ParseResult<Expr>
    where
        F: FnMut(&mut Self, bool) -> ParseResult<Expr>,
    {
        let mut expr = operand(self, struct_ok)?;

        while self.matches(operators) {
            let operator = self.advance();
            let right = operand(self, struct_ok)?;
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
        Parser {
            tokens,
            neu,
            function_nesting_depth: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::scanner::Scanner;

    fn parse_stmts(src: &str) -> Vec<Stmt> {
        let mut neu = Neu::new();
        let (toks, _) = Scanner::scan(src);
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

    #[test]
    fn parses_for_loop_desugars_to_while() {
        let stmts = parse_stmts("for i in 0..3 { }");
        assert_eq!(stmts.len(), 1);

        match &stmts[0] {
            Stmt::Block(outer) => {
                assert_eq!(outer.len(), 2);
                assert!(matches!(outer[0], Stmt::Expr(_)));

                if let Stmt::While { condition, body } = &outer[1] {
                    assert!(matches!(condition, Expr::Binary { .. }));
                    if let Stmt::Block(inner) = &**body {
                        assert!(inner.last().is_some_and(|s| matches!(s, Stmt::Expr(_))));
                    } else {
                        panic!("expected block in while body");
                    }
                } else {
                    panic!("expected while-stmt");
                }
            }
            _ => panic!("expected outer block generated by for-desugaring"),
        }
    }

    #[test]
    fn parses_function_declaration_and_call() {
        let stmts = parse_stmts("fn add(a, b) { return a + b; } add(1, 2);");
        assert!(matches!(
            stmts.as_slice(),
            [Stmt::FunctionDecl(_), Stmt::Expr(Expr::Call { .. })]
        ));
    }

    #[test]
    fn disallow_return_at_top_level() {
        let mut neu = Neu::new();
        let (toks, _) = Scanner::scan("return 1;");
        let _stmts = Parser::parse(toks, &mut neu);
        assert!(neu.had_error, "Expected an error for top-level return");
    }

    #[test]
    fn allow_return_in_function() {
        let mut neu = Neu::new();
        let (toks, _) = Scanner::scan("fn foo() { return 1; }");
        let stmts = Parser::parse(toks, &mut neu);
        assert!(
            !neu.had_error,
            "Did not expect an error for return in function"
        );
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::FunctionDecl(decl) => {
                assert_eq!(decl.body.len(), 1);
                match &decl.body[0] {
                    Stmt::Return { value } => assert!(value.is_some()),
                    _ => panic!("Expected return statement in function body"),
                }
            }
            _ => panic!("Expected function declaration"),
        }
    }
}
