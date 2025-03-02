use std::iter::Peekable;
use std::str::Chars;

use crate::token::{LiteralValue, Token};
use crate::Neu;

#[derive(Debug)]
pub struct Scanner<'a, 'b> {
    source: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    neu: &'b mut Neu,
    line: usize,
}

impl<'a, 'b> Scanner<'a, 'b> {
    pub fn new(source: &'a str, neu: &'b mut Neu) -> Self {
        Scanner {
            source: source.chars().peekable(),
            tokens: Vec::new(),
            neu,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while let Some(character) = self.advance() {
            let ch = character.into();
            match character {
                '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => self.add_token(ch),
                '!' | '=' | '<' | '>' => {
                    let token = if self.matches('=') { ch + "=" } else { ch };
                    self.add_token(token);
                }
                '/' => {
                    if self.matches('/') {
                        self.consume_while(|c| c != '\n');
                    } else {
                        self.add_token(ch)
                    }
                }
                '\n' => self.line += 1,
                '"' => self.scan_string(),
                character if character.is_whitespace() => {}
                character if character.is_ascii_digit() => self.scan_number(ch),
                character if character.is_alphabetic() => self.scan_identifier(ch),
                _ => self.neu.error(self.line, "Unexpected character.".into()),
            }
        }

        self.add_token("".into());
        &self.tokens
    }

    fn scan_string(&mut self) {
        let value = self.consume_while(|c| c != '"');

        if self.advance().is_none() {
            self.neu.error(self.line, "Unterminated string.".into());
        } else {
            self.add_token_literal(format!("\"{}\"", value), LiteralValue::String(value));
        }
    }

    fn scan_number(&mut self, mut num_str: String) {
        num_str.push_str(self.consume_while(|c| c.is_ascii_digit()).as_str());

        if self.peek() == Some(&'.') && self.peek_next().map_or(false, |c| c.is_ascii_digit()) {
            num_str.push(self.advance().unwrap());
            num_str.push_str(self.consume_while(|c| c.is_ascii_digit()).as_str());
        }

        let num = num_str.parse::<f64>().unwrap();
        self.add_token_literal(num_str, LiteralValue::Number(num));
    }

    fn scan_identifier(&mut self, mut identifier: String) {
        identifier.push_str(self.consume_while(|c| c.is_alphanumeric()).as_str());
        self.add_token(identifier)
    }

    fn add_token(&mut self, lexeme: String) {
        self.add_token_literal(lexeme, LiteralValue::None);
    }

    fn add_token_literal(&mut self, lexeme: String, literal: LiteralValue) {
        self.tokens.push(Token::new(lexeme, literal, self.line));
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

    fn matches(&mut self, expected: char) -> bool {
        self.peek().map_or(false, |c| *c == expected) && {
            self.advance();
            true
        }
    }

    fn consume_while<F>(&mut self, predicate: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut buf = String::new();
        while self.peek().map_or(false, |&c| predicate(c)) {
            let ch = self.advance().unwrap();
            if ch == '\n' {
                self.line += 1;
            }
            buf.push(ch);
        }
        buf
    }
}
