use crate::frontend::literal::Literal;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    DotDot,
    Minus,
    Plus,
    Modulo,
    Colon,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    And,
    Struct,
    Else,
    False,
    Fn,
    For,
    If,
    In,
    None,
    Or,
    Return,
    This,
    True,
    While,
    Break,
    Continue,

    Identifier,
    String,
    Number,

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenType, lexeme: String, literal: Option<Literal>, line: usize) -> Self {
        Token {
            kind,
            lexeme,
            literal,
            line,
        }
    }
}
