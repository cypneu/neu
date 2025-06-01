use std::iter::Peekable;
use std::str::Chars;

use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};

#[derive(Debug)]
pub struct ScanError {
    pub line: usize,
    pub message: String,
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    errors: Vec<ScanError>,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn scan(source: &'a str) -> (Vec<Token>, Vec<ScanError>) {
        let scanner = Scanner::new(source);
        scanner.scan_tokens()
    }

    fn scan_tokens(mut self) -> (Vec<Token>, Vec<ScanError>) {
        while let Some(character) = self.advance() {
            let ch = character.into();
            match character {
                '.' => {
                    if self.consume_char_if('.') {
                        self.add_token(TokenType::DotDot, "..");
                    } else {
                        self.add_token(TokenType::Dot, ch);
                    }
                }
                '(' => self.add_token(TokenType::LeftParen, ch),
                ')' => self.add_token(TokenType::RightParen, ch),
                '{' => self.add_token(TokenType::LeftBrace, ch),
                '}' => self.add_token(TokenType::RightBrace, ch),
                ',' => self.add_token(TokenType::Comma, ch),
                '-' => self.add_token(TokenType::Minus, ch),
                '+' => self.add_token(TokenType::Plus, ch),
                ';' => self.add_token(TokenType::Semicolon, ch),
                ':' => self.add_token(TokenType::Colon, ch),
                '*' => self.add_token(TokenType::Star, ch),
                '%' => self.add_token(TokenType::Modulo, ch),
                '!' => {
                    if self.consume_char_if('=') {
                        self.add_token(TokenType::BangEqual, "!=");
                    } else {
                        self.add_token(TokenType::Bang, ch);
                    }
                }
                '=' => {
                    if self.consume_char_if('=') {
                        self.add_token(TokenType::EqualEqual, "==");
                    } else {
                        self.add_token(TokenType::Equal, ch);
                    }
                }
                '<' => {
                    if self.consume_char_if('=') {
                        self.add_token(TokenType::LessEqual, "<=");
                    } else {
                        self.add_token(TokenType::Less, ch);
                    }
                }
                '>' => {
                    if self.consume_char_if('=') {
                        self.add_token(TokenType::GreaterEqual, ">=");
                    } else {
                        self.add_token(TokenType::Greater, ch);
                    };
                }
                '/' => {
                    if self.consume_char_if('/') {
                        self.consume_while(|c| c != '\n');
                    } else {
                        self.add_token(TokenType::Slash, ch)
                    }
                }
                '\n' => self.line += 1,
                '"' => self.scan_string(),
                character if character.is_whitespace() => {}
                character if character.is_ascii_digit() => self.scan_number(ch),
                character if character.is_alphabetic() || ch == "_" => self.scan_identifier(ch),
                character => self.error(self.line, format!("Unexpected character '{}'", character)),
            }
        }

        self.add_token(TokenType::Eof, "");
        (self.tokens, self.errors)
    }

    fn error(&mut self, line: usize, message: String) {
        self.errors.push(ScanError { line, message });
    }

    fn scan_string(&mut self) {
        let value = self.consume_while(|c| c != '"');

        if self.advance().is_none() {
            self.error(self.line, "Unterminated string.".into());
        } else {
            let lexeme = format!("\"{}\"", value);
            self.add_token_literal(TokenType::String, lexeme, Some(Literal::String(value)));
        }
    }

    fn scan_number(&mut self, mut num_str: String) {
        num_str.push_str(self.consume_while(|c| c.is_ascii_digit()).as_str());

        if self.peek() == Some(&'.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            num_str.push(self.advance().unwrap());
            num_str.push_str(self.consume_while(|c| c.is_ascii_digit()).as_str());
        }

        let num = num_str.parse::<f64>().unwrap();
        self.add_token_literal(TokenType::Number, num_str, Some(Literal::Number(num)));
    }

    fn scan_identifier(&mut self, mut identifier: String) {
        identifier.push_str(
            self.consume_while(|c| c.is_alphanumeric() || c == '_')
                .as_str(),
        );

        let (kind, literal_opt) = match identifier.as_str() {
            "and" => (TokenType::And, None),
            "struct" => (TokenType::Struct, None),
            "else" => (TokenType::Else, None),
            "false" => (TokenType::False, Some(Literal::Boolean(false))),
            "fn" => (TokenType::Fn, None),
            "for" => (TokenType::For, None),
            "if" => (TokenType::If, None),
            "in" => (TokenType::In, None),
            "none" => (TokenType::None, Some(Literal::None)),
            "or" => (TokenType::Or, None),
            "return" => (TokenType::Return, None),
            "this" => (TokenType::This, None),
            "true" => (TokenType::True, Some(Literal::Boolean(true))),
            "while" => (TokenType::While, None),
            _ => (TokenType::Identifier, None),
        };

        self.add_token_literal(kind, identifier, literal_opt);
    }

    fn new(source: &'a str) -> Self {
        Scanner {
            source: source.chars().peekable(),
            tokens: Vec::new(),
            errors: Vec::new(),
            line: 1,
        }
    }

    fn consume_char_if(&mut self, expected: char) -> bool {
        self.peek().is_some_and(|c| *c == expected) && {
            self.advance();
            true
        }
    }

    fn consume_while<F>(&mut self, predicate: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut buf = String::new();
        while self.peek().is_some_and(|&c| predicate(c)) {
            let ch = self.advance().unwrap();
            if ch == '\n' {
                self.line += 1;
            }
            buf.push(ch);
        }
        buf
    }

    fn add_token(&mut self, kind: TokenType, lexeme: impl Into<String>) {
        self.add_token_literal(kind, lexeme, None);
    }

    fn add_token_literal(
        &mut self,
        kind: TokenType,
        lexeme: impl Into<String>,
        lit: Option<Literal>,
    ) {
        self.tokens
            .push(Token::new(kind, lexeme.into(), lit, self.line));
    }

    fn advance(&mut self) -> Option<char> {
        self.source.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.source.peek()
    }

    fn peek_next(&mut self) -> Option<char> {
        self.source.clone().nth(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::token::TokenType;

    fn assert_tokens_properties(
        tokens: &[Token],
        expected_tokens_without_eof: &[(TokenType, &str, Option<Literal>)],
    ) {
        assert_eq!(
            tokens.len(),
            expected_tokens_without_eof.len() + 1,
            "Number of tokens mismatch. Got (incl EOF): {}, Expected (excl EOF): {}.\nActual Tokens (kind only): {:?}\nExpected lexemes: {:?}",
            tokens.len(),
            expected_tokens_without_eof.len(),
            tokens.iter().map(|t| t.kind.clone()).collect::<Vec<_>>(),
            expected_tokens_without_eof.iter().map(|e| e.1).collect::<Vec<_>>()
        );

        for (i, exp) in expected_tokens_without_eof.iter().enumerate() {
            let token = &tokens[i];
            assert_eq!(
                token.kind, exp.0,
                "Token index {} kind mismatch. Got {:?}, Expected {:?}",
                i, token.kind, exp.0
            );
            assert_eq!(
                token.lexeme, exp.1,
                "Token index {} lexeme mismatch. Got '{}', Expected '{}'",
                i, token.lexeme, exp.1
            );
            assert_eq!(
                token.literal, exp.2,
                "Token index {} literal mismatch. Got {:?}, Expected {:?}",
                i, token.literal, exp.2
            );
        }

        assert_eq!(
            tokens.last().expect("Token list should not be empty").kind,
            TokenType::Eof,
            "Last token must be EOF"
        );
    }

    #[test]
    fn empty_source() {
        let (tokens, _scan_errors) = Scanner::scan("");
        assert_tokens_properties(&tokens, &[]);
        assert_eq!(tokens[0].line, 1, "EOF token line number for empty source");
    }

    #[test]
    fn single_char_punctuators() {
        let (tokens, _scan_errors) = Scanner::scan("(){},.-+;*: %");
        let expected = vec![
            (TokenType::LeftParen, "(", None),
            (TokenType::RightParen, ")", None),
            (TokenType::LeftBrace, "{", None),
            (TokenType::RightBrace, "}", None),
            (TokenType::Comma, ",", None),
            (TokenType::Dot, ".", None),
            (TokenType::Minus, "-", None),
            (TokenType::Plus, "+", None),
            (TokenType::Semicolon, ";", None),
            (TokenType::Star, "*", None),
            (TokenType::Colon, ":", None),
            (TokenType::Modulo, "%", None),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn operators_multi_char() {
        let source = "! != = == > >= < <= .. /";
        let (tokens, _scan_errors) = Scanner::scan(source);
        let expected = vec![
            (TokenType::Bang, "!", None),
            (TokenType::BangEqual, "!=", None),
            (TokenType::Equal, "=", None),
            (TokenType::EqualEqual, "==", None),
            (TokenType::Greater, ">", None),
            (TokenType::GreaterEqual, ">=", None),
            (TokenType::Less, "<", None),
            (TokenType::LessEqual, "<=", None),
            (TokenType::DotDot, "..", None),
            (TokenType::Slash, "/", None),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn string_literals() {
        let (tokens, _scan_errors) = Scanner::scan("\"foo\" \"\" \"hello world\"");
        let expected = vec![
            (
                TokenType::String,
                "\"foo\"",
                Some(Literal::String("foo".to_string())),
            ),
            (
                TokenType::String,
                "\"\"",
                Some(Literal::String("".to_string())),
            ),
            (
                TokenType::String,
                "\"hello world\"",
                Some(Literal::String("hello world".to_string())),
            ),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn number_literals() {
        let (tokens, _scan_errors) = Scanner::scan("123 123.45 0 0.0");
        let expected = vec![
            (TokenType::Number, "123", Some(Literal::Number(123.0))),
            (TokenType::Number, "123.45", Some(Literal::Number(123.45))),
            (TokenType::Number, "0", Some(Literal::Number(0.0))),
            (TokenType::Number, "0.0", Some(Literal::Number(0.0))),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn number_literal_edge_cases() {
        let (tokens, _scan_errors) = Scanner::scan(".5 5.");
        let expected = vec![
            (TokenType::Dot, ".", None),
            (TokenType::Number, "5", Some(Literal::Number(5.0))),
            (TokenType::Number, "5", Some(Literal::Number(5.0))),
            (TokenType::Dot, ".", None),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn identifiers() {
        let source = "foo _bar foo_bar F_O_O123 foo1 id";
        let (tokens, _scan_errors) = Scanner::scan(source);
        let expected = vec![
            (TokenType::Identifier, "foo", None),
            (TokenType::Identifier, "_bar", None),
            (TokenType::Identifier, "foo_bar", None),
            (TokenType::Identifier, "F_O_O123", None),
            (TokenType::Identifier, "foo1", None),
            (TokenType::Identifier, "id", None),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn keywords() {
        let source = "and struct else false fn for if in none or return this true while";
        let (tokens, _scan_errors) = Scanner::scan(source);
        let expected = vec![
            (TokenType::And, "and", None),
            (TokenType::Struct, "struct", None),
            (TokenType::Else, "else", None),
            (TokenType::False, "false", Some(Literal::Boolean(false))),
            (TokenType::Fn, "fn", None),
            (TokenType::For, "for", None),
            (TokenType::If, "if", None),
            (TokenType::In, "in", None),
            (TokenType::None, "none", Some(Literal::None)),
            (TokenType::Or, "or", None),
            (TokenType::Return, "return", None),
            (TokenType::This, "this", None),
            (TokenType::True, "true", Some(Literal::Boolean(true))),
            (TokenType::While, "while", None),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn comments_are_ignored() {
        let source = "// this is a comment\nident // another comment\n// last line comment";
        let (tokens, _scan_errors) = Scanner::scan(source);
        let expected = vec![(TokenType::Identifier, "ident", None)];
        assert_tokens_properties(&tokens, &expected);
        assert_eq!(tokens[0].line, 2, "Token 'ident' should be on line 2");
        assert_eq!(
            tokens.last().unwrap().line,
            3,
            "EOF token should be on line after last comment line"
        );
    }

    #[test]
    fn whitespace_and_line_numbers() {
        let source = "one\n  two //c\n\nthree\n";
        let (tokens, _scan_errors) = Scanner::scan(source);
        let expected = vec![
            (TokenType::Identifier, "one", None),
            (TokenType::Identifier, "two", None),
            (TokenType::Identifier, "three", None),
        ];
        assert_tokens_properties(&tokens, &expected);
        assert_eq!(tokens[0].line, 1, "Token 'one' line number");
        assert_eq!(tokens[1].line, 2, "Token 'two' line number");
        assert_eq!(tokens[2].line, 4, "Token 'three' line number");
        assert_eq!(
            tokens.last().unwrap().line,
            5,
            "EOF token line number (after trailing newline)"
        );
    }

    #[test]
    fn error_unterminated_string() {
        let (tokens, scan_errors) = Scanner::scan("\"abc");
        assert!(
            !scan_errors.is_empty(),
            "Expected error for unterminated string"
        );
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenType::Eof);
    }

    #[test]
    fn error_invalid_character() {
        let (tokens, scan_errors) = Scanner::scan("a = @;");
        assert!(
            !scan_errors.is_empty(),
            "Expected error for invalid character"
        );
        let expected = vec![
            (TokenType::Identifier, "a", None),
            (TokenType::Equal, "=", None),
            (TokenType::Semicolon, ";", None),
        ];
        assert_tokens_properties(&tokens, &expected);
    }

    #[test]
    fn mixed_tokens_sequence() {
        let source = "fn main() {\n  val x = 10.5; // comment\n  return x > 0;\n}";
        let (tokens, scan_errors) = Scanner::scan(source);
        assert!(
            scan_errors.is_empty(),
            "Did not expect errors in mixed sequence. Errors: {:?}",
            scan_errors
        );

        let expected = vec![
            (TokenType::Fn, "fn", None), // line 1
            (TokenType::Identifier, "main", None),
            (TokenType::LeftParen, "(", None),
            (TokenType::RightParen, ")", None),
            (TokenType::LeftBrace, "{", None),
            (TokenType::Identifier, "val", None),
            (TokenType::Identifier, "x", None),
            (TokenType::Equal, "=", None),
            (TokenType::Number, "10.5", Some(Literal::Number(10.5))),
            (TokenType::Semicolon, ";", None),
            (TokenType::Return, "return", None),
            (TokenType::Identifier, "x", None),
            (TokenType::Greater, ">", None),
            (TokenType::Number, "0", Some(Literal::Number(0.0))),
            (TokenType::Semicolon, ";", None),
            (TokenType::RightBrace, "}", None),
        ];
        assert_tokens_properties(&tokens, &expected);

        assert_eq!(tokens[0].line, 1, "fn line");
        assert_eq!(tokens[5].line, 2, "val line");
        assert_eq!(tokens[9].line, 2, "; after 10.5 line");
        assert_eq!(tokens[10].line, 3, "return line");
        assert_eq!(tokens[15].line, 4, "}} line");
        assert_eq!(tokens.last().unwrap().line, 4, "EOF line");
    }
}
