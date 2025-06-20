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
pub struct Token<'src> {
    pub kind: TokenType,
    pub lexeme: &'src str,
    pub literal: Option<Literal<'src>>,
    pub line: usize,
}

impl<'src> Token<'src> {
    pub fn new(
        kind: TokenType,
        lexeme: &'src str,
        literal: Option<Literal<'src>>,
        line: usize,
    ) -> Self {
        Token {
            kind,
            lexeme,
            literal,
            line,
        }
    }
}
