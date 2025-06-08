use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

use crate::ast::expr::Expr;
use crate::ast::stmt::{FunctionDecl, Stmt};
use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};

const EQ_OPS: [TokenType; 2] = [TokenType::EqualEqual, TokenType::BangEqual];
const CMP_OPS: [TokenType; 4] = [
    TokenType::Greater,
    TokenType::GreaterEqual,
    TokenType::Less,
    TokenType::LessEqual,
];
const TERM_OPS: [TokenType; 2] = [TokenType::Minus, TokenType::Plus];
const FACTOR_OPS: [TokenType; 3] = [TokenType::Slash, TokenType::Star, TokenType::Modulo];
const UNARY_OPS: [TokenType; 2] = [TokenType::Bang, TokenType::Minus];

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
    pub location: String,
    pub message: String,
}

impl ParseError {
    pub fn new(token: &Token, location: String, message: &str) -> Self {
        Self {
            message: message.to_string(),
            location,
            token: token.clone(),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    function_nesting_depth: u32,
    loop_nesting_depth: u32,
}

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> (Vec<Stmt>, Vec<ParseError>) {
        let mut parser = Parser::new(tokens);

        let (mut statements, mut errors) = (Vec::new(), Vec::new());
        while !parser.check(TokenType::Eof) {
            match parser.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => errors.push(err),
            }
        }

        (statements, errors)
    }
}

impl Parser {
    fn declaration(&mut self) -> ParseResult<Stmt> {
        let result = match self.peek().kind {
            TokenType::Struct => self.struct_declaration(),
            TokenType::Fn => self.function_declaration(),
            _ => self.statement(),
        };

        if result.is_err() {
            self.synchronize();
        }
        result
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        match self.peek().kind {
            TokenType::LeftBrace => self.block(),
            TokenType::If => self.if_statement(),
            TokenType::While => self.while_statement(),
            TokenType::For => self.for_statement(),
            TokenType::Return => self.return_statement(),
            TokenType::Break => self.break_statement(),
            TokenType::Continue => self.continue_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;

        let msg = "Expect ';' after expression statement.";
        self.consume(TokenType::Semicolon, msg)?;

        Ok(Stmt::Expr(expr))
    }

    fn block(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftBrace, "Expected '{' to start a block.")?;
        let mut statements = Vec::new();

        while !self.check_any(&[TokenType::RightBrace, TokenType::Eof]) {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block")?;
        Ok(Stmt::Block(statements))
    }

    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::If, "Expected 'if'.")?;
        let condition = self.or(false)?;
        let then_branch = self.block()?;

        let else_branch = if self.consume_if(TokenType::Else) {
            if self.check(TokenType::If) {
                Some(Box::new(self.if_statement()?))
            } else {
                Some(Box::new(self.block()?))
            }
        } else {
            None
        };

        Ok(Stmt::if_stmt(
            condition,
            then_branch,
            else_branch.map(|b| *b),
        ))
    }

    fn while_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::While, "Expected 'while'.")?;
        let condition = self.or(false)?;
        self.loop_nesting_depth += 1;
        let body = self.block()?;
        self.loop_nesting_depth -= 1;
        Ok(Stmt::while_stmt(condition, body))
    }

    fn for_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::For, "Expeced 'for'.")?;
        let var = self.consume(TokenType::Identifier, "Expected loop variable")?;
        self.consume(TokenType::In, "Expected 'in' in for loop")?;
        let start = self.unary(false)?;
        self.consume(TokenType::DotDot, "Expected '..' in range for loop")?;
        let end = self.unary(false)?;

        self.loop_nesting_depth += 1;
        let body = self.block()?;
        self.loop_nesting_depth -= 1;

        Ok(Stmt::for_stmt(var, start, end, body))
    }

    fn return_statement(&mut self) -> ParseResult<Stmt> {
        let keyword = self.consume(TokenType::Return, "Expected 'return'.")?;
        if self.function_nesting_depth == 0 {
            return Err(self.error(&keyword, "Return can be used only within a function."));
        }

        let value = if !self.check(TokenType::Semicolon) {
            Some(self.or(true)?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expected ';' after return value.")?;
        Ok(Stmt::Return { value })
    }

    fn break_statement(&mut self) -> ParseResult<Stmt> {
        let keyword = self.consume(TokenType::Break, "Expected 'break'.")?;
        if self.loop_nesting_depth == 0 {
            return Err(self.error(&keyword, "Break can be used only within a loop."));
        }

        self.consume(TokenType::Semicolon, "Expected ';' after break statement.")?;
        Ok(Stmt::Break)
    }

    fn continue_statement(&mut self) -> ParseResult<Stmt> {
        let keyword = self.consume(TokenType::Continue, "Expected 'break'.")?;
        if self.loop_nesting_depth == 0 {
            return Err(self.error(&keyword, "Continue can be used only within a loop."));
        }

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after continue statement.",
        )?;
        Ok(Stmt::Continue)
    }

    fn struct_declaration(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::Struct, "Expected 'struct'.")?;
        let name = self.consume(TokenType::Identifier, "Expected struct name")?;
        self.consume(TokenType::LeftBrace, "Expected '{' before struct body")?;

        let mut fields = Vec::new();
        let mut field_names_seen = std::collections::HashSet::new();

        while self.check(TokenType::Identifier) {
            let field_token = self.advance();
            if !field_names_seen.insert(field_token.lexeme.clone()) {
                let msg = format!("Duplicate field name '{}'.", field_token.lexeme);
                return Err(self.error(&field_token, &msg));
            }

            fields.push(field_token);
            if self.consume_if(TokenType::Comma) {
                continue;
            }

            if self.check(TokenType::RightBrace) {
                break;
            }

            return Err(self.error(&name, "Expected ',' before method declarations"));
        }

        let mut methods = Vec::new();
        while !self.check_any(&[TokenType::RightBrace, TokenType::Eof]) {
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
        self.consume(TokenType::Fn, &format!("Expected '{}'.", kind))?;
        let name = self.consume(TokenType::Identifier, &format!("Expected {} name.", kind))?;

        let params = self.parse_parameter_list(kind)?;

        self.function_nesting_depth += 1;
        let body = match self.block()? {
            Stmt::Block(stmts) => stmts,
            _ => unreachable!(),
        };
        self.function_nesting_depth -= 1;

        Ok(FunctionDecl { name, params, body })
    }

    fn parse_parameter_list(&mut self, kind: &str) -> ParseResult<Vec<Token>> {
        self.consume(
            TokenType::LeftParen,
            &format!("Expected '(' in {} declaration", kind),
        )?;

        let mut params = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    let token = self.peek().clone();
                    return Err(self.error(&token, "Can't have more than 255 parameters."));
                }

                params.push(self.consume(TokenType::Identifier, "Expect parameter name.")?);
                if !self.consume_if(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            &format!("Expected ')' in {} declaration", kind),
        )?;
        Ok(params)
    }
}

impl Parser {
    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let lhs = self.or(true)?;
        if !self.check(TokenType::Equal) {
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

        while self.check(TokenType::Or) {
            let op = self.advance();
            let right = self.and(struct_ok)?;
            expr = Expr::logical(expr, op, right);
        }

        Ok(expr)
    }

    fn and(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        let mut expr = self.equality(struct_ok)?;

        while self.check(TokenType::And) {
            let op = self.advance();
            let right = self.equality(struct_ok)?;
            expr = Expr::logical(expr, op, right)
        }

        Ok(expr)
    }

    fn equality(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::comparison, struct_ok, &EQ_OPS)
    }

    fn comparison(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::term, struct_ok, &CMP_OPS)
    }

    fn term(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::factor, struct_ok, &TERM_OPS)
    }

    fn factor(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        self.binary_expr(Self::unary, struct_ok, &FACTOR_OPS)
    }

    fn unary(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        if self.check_any(&UNARY_OPS) {
            let operator = self.advance();
            let right = self.unary(struct_ok)?;
            return Ok(Expr::unary(operator, right));
        }

        self.call(struct_ok)
    }

    fn call(&mut self, struct_ok: bool) -> ParseResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.consume_if(TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.consume_if(TokenType::Dot) {
                let name =
                    self.consume(TokenType::Identifier, "Expect property name after '.'.")?;
                expr = Expr::get(expr, name);
            } else if struct_ok && self.consume_if(TokenType::LeftBrace) {
                expr = self.struct_literal(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let token = self.peek().clone();
                    return Err(self.error(&token, "Can't have more than 255 arguments."));
                }

                arguments.push(self.or(true)?);
                if !self.consume_if(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(Expr::call(callee, arguments))
    }

    fn struct_literal(&mut self, initializer: Expr) -> ParseResult<Expr> {
        let mut fields = Vec::<(Token, Expr)>::new();
        if !self.check(TokenType::RightBrace) {
            loop {
                let name = self.consume(TokenType::Identifier, "Expected field name.")?;
                self.consume(TokenType::Colon, "Expected ':' after field name.")?;
                let value = self.or(true)?;
                fields.push((name, value));
                if !self.consume_if(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightBrace, "Expected '}' after struct literal.")?;
        Ok(Expr::struct_init(initializer, fields))
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        let token = self.advance();
        let expr = match token.kind {
            TokenType::False => false.into(),
            TokenType::True => true.into(),
            TokenType::None => Literal::None.into(),
            TokenType::Number | TokenType::String => token.literal.unwrap().into(),
            TokenType::LeftParen => {
                let expression = self.or(true)?;
                self.consume(TokenType::RightParen, "Expected ')' after expression")?;
                Expr::group(expression)
            }
            TokenType::Identifier => Expr::variable(token),
            _ => return Err(self.error(&token, "Expected expression")),
        };
        Ok(expr)
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

        while self.check_any(operators) {
            let operator = self.advance();
            let right = operand(self, struct_ok)?;
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }
}

impl Parser {
    fn error(&mut self, token: &Token, message: &str) -> ParseError {
        let location = match token.kind {
            TokenType::Eof => " at end".into(),
            _ => format!(" at '{}'", token.lexeme),
        };
        ParseError::new(token, location, message)
    }

    fn synchronize(&mut self) {
        while !self.check(TokenType::Eof) {
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
                | TokenType::Return
                | TokenType::Continue
                | TokenType::Break => return,
                _ => self.advance(),
            };
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.peek().kind == kind {
            Ok(self.advance())
        } else {
            let token = self.peek().clone();
            Err(self.error(&token, message))
        }
    }

    fn consume_if_any(&mut self, kinds: &[TokenType]) -> bool {
        if kinds.contains(&self.peek().kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if(&mut self, kind: TokenType) -> bool {
        self.consume_if_any(&[kind])
    }

    fn check_any(&mut self, kinds: &[TokenType]) -> bool {
        kinds.contains(&self.peek().kind)
    }

    fn check(&mut self, kind: TokenType) -> bool {
        self.peek().kind == kind
    }

    fn advance(&mut self) -> Token {
        self.tokens.next().expect("scanner guaranteed EOF")
    }

    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn new(tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().peekable();
        Parser {
            tokens,
            function_nesting_depth: 0,
            loop_nesting_depth: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::scanner::Scanner;

    fn parse_stmts(src: &str) -> Vec<Stmt> {
        let (toks, _) = Scanner::scan(src);
        Parser::parse(toks).0
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
    fn parses_for_loop() {
        let stmts = parse_stmts("for i in 0..3 { }");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
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
        let (toks, _) = Scanner::scan("return 1;");
        let (_stmts, errors) = Parser::parse(toks);
        assert!(!errors.is_empty(), "Expected an error for top-level return");
    }

    #[test]
    fn allow_return_in_function() {
        let (toks, _) = Scanner::scan("fn foo() { return 1; }");
        let (stmts, errors) = Parser::parse(toks);
        assert!(
            errors.is_empty(),
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

    #[test]
    fn parse_struct_declaration_empty() {
        let stmts = parse_stmts("struct Foo {}");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::StructDecl(decl) => {
                assert_eq!(decl.name.lexeme, "Foo");
                assert!(decl.fields.is_empty());
                assert!(decl.methods.is_empty());
            }
            _ => panic!("Expected StructDecl statement"),
        }
    }

    #[test]
    fn parse_struct_declaration_with_fields() {
        let stmts = parse_stmts("struct Point { x, y }");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::StructDecl(decl) => {
                assert_eq!(decl.name.lexeme, "Point");
                assert_eq!(decl.fields.len(), 2);
                assert_eq!(decl.fields[0].lexeme, "x");
                assert_eq!(decl.fields[1].lexeme, "y");
                assert!(decl.methods.is_empty());
            }
            _ => panic!("Expected StructDecl statement"),
        }
    }

    #[test]
    fn parse_struct_declaration_with_methods() {
        let stmts = parse_stmts("struct Greeter { fn greet() {} fn say(msg) {} }");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::StructDecl(decl) => {
                assert_eq!(decl.name.lexeme, "Greeter");
                assert!(decl.fields.is_empty());
                assert_eq!(decl.methods.len(), 2);
                assert_eq!(decl.methods[0].name.lexeme, "greet");
                assert_eq!(decl.methods[1].name.lexeme, "say");
            }
            _ => panic!("Expected StructDecl statement"),
        }
    }

    #[test]
    fn parse_struct_declaration_with_fields_and_methods() {
        let stmts = parse_stmts("struct Data { field1, field2, fn process() {} fn query() {} }");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::StructDecl(decl) => {
                assert_eq!(decl.name.lexeme, "Data");
                assert_eq!(decl.fields.len(), 2);
                assert_eq!(decl.fields[0].lexeme, "field1");
                assert_eq!(decl.fields[1].lexeme, "field2");
                assert_eq!(decl.methods.len(), 2);
                assert_eq!(decl.methods[0].name.lexeme, "process");
                assert_eq!(decl.methods[1].name.lexeme, "query");
            }
            _ => panic!("Expected StructDecl statement"),
        }
    }

    #[test]
    fn parse_struct_init_empty() {
        let expr = parse_expr("MyStruct {};");
        match expr {
            Expr::StructInit {
                initializer,
                fields,
            } => {
                assert!(
                    matches!(*initializer, Expr::Variable { name } if name.lexeme == "MyStruct")
                );
                assert!(fields.is_empty());
            }
            _ => panic!("Expected StructInit expression"),
        }
    }

    #[test]
    fn parse_struct_init_with_fields() {
        let expr = parse_expr("Point { x: 1, y: \"two\" };");
        match expr {
            Expr::StructInit {
                initializer,
                fields,
            } => {
                assert!(matches!(*initializer, Expr::Variable { name } if name.lexeme == "Point"));
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0.lexeme, "x");
                assert!(matches!(fields[0].1, Expr::Literal(Literal::Number(_))));
                assert_eq!(fields[1].0.lexeme, "y");
                assert!(matches!(fields[1].1, Expr::Literal(Literal::String(_))));
            }
            _ => panic!("Expected StructInit expression"),
        }
    }

    #[test]
    fn parse_error_missing_semicolon() {
        let (toks, _) = Scanner::scan("x = 1");
        let (_, errors) = Parser::parse(toks);
        assert!(!errors.is_empty());
        assert!(errors[0]
            .message
            .contains("Expect ';' after expression statement."));
    }

    #[test]
    fn parse_max_parameters_allowed() {
        let params = (0..255)
            .map(|i| format!("p{}", i))
            .collect::<Vec<_>>()
            .join(", ");
        let src = format!("fn test({}) {{}}", params);
        let (toks, _) = Scanner::scan(&src);
        let (_, errors) = Parser::parse(toks);
        assert!(
            errors.is_empty(),
            "Should allow 255 parameters. Errors: {:?}",
            errors
        );
    }

    #[test]
    fn parse_too_many_parameters_error() {
        let params = (0..256)
            .map(|i| format!("p{}", i))
            .collect::<Vec<_>>()
            .join(", ");
        let src = format!("fn test({}) {{}}", params);
        let (toks, _) = Scanner::scan(&src);
        let (_, errors) = Parser::parse(toks);
        assert!(!errors.is_empty());
        assert!(errors[0]
            .message
            .contains("Can't have more than 255 parameters."));
    }

    #[test]
    fn parse_error_struct_field_no_comma_before_method() {
        let (toks, _) = Scanner::scan("struct Test { field fn method() {} }");
        let (_, errors) = Parser::parse(toks);
        assert!(!errors.is_empty());
        assert!(errors[0]
            .message
            .contains("Expected ',' before method declarations"));
    }

    #[test]
    fn parse_error_struct_field_repeated_variables() {
        let (toks, _) = Scanner::scan("struct Test { field, field }");
        let (_, errors) = Parser::parse(toks);
        assert!(!errors.is_empty());
        assert!(errors[0].message.contains("Duplicate field name 'field'."));
    }

    #[test]
    fn disallow_assignment_in_if_condition() {
        let src = "if x = 1 { }";
        let (toks, _) = Scanner::scan(src);
        let (_, errors) = Parser::parse(toks);
        println!("{:?}", errors);

        assert!(
            !errors.is_empty(),
            "Expected a parse error for assignment in if condition"
        );
    }

    #[test]
    fn disallow_assignment_in_while_condition() {
        let src = "while (x = 0) { }";
        let (toks, _) = Scanner::scan(src);
        let (_, errors) = Parser::parse(toks);

        assert!(
            !errors.is_empty(),
            "Expected a parse error for assignment in while condition"
        );
    }

    #[test]
    fn parses_break_statement_in_loop() {
        let stmts = parse_stmts("while true { break; }");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::While { body, .. } => match &**body {
                Stmt::Block(block_stmts) => {
                    assert_eq!(block_stmts.len(), 1);
                    assert!(matches!(block_stmts[0], Stmt::Break));
                }
                _ => panic!("Expected block statement for while body"),
            },
            _ => panic!("Expected a while statement"),
        }
    }

    #[test]
    fn parses_continue_statement_in_loop() {
        let stmts = parse_stmts("for i in 0..5 { continue; }");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::For { body, .. } => match &**body {
                Stmt::Block(block_stmts) => {
                    assert!(matches!(block_stmts[0], Stmt::Continue));
                }
                _ => panic!("Expected block statement for while body"),
            },
            _ => panic!("Expected a For statement"),
        }
    }
    #[test]
    fn error_on_break_outside_loop() {
        let (toks, _) = Scanner::scan("break;");
        let (_, errors) = Parser::parse(toks);
        assert!(!errors.is_empty());
        assert!(errors[0]
            .message
            .contains("Break can be used only within a loop."));
    }

    #[test]
    fn error_on_continue_outside_loop() {
        let (toks, _) = Scanner::scan("continue;");
        let (_, errors) = Parser::parse(toks);
        assert!(!errors.is_empty());
        assert!(errors[0]
            .message
            .contains("Continue can be used only within a loop."));
    }
}
