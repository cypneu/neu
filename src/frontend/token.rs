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
    Super,
    This,
    True,
    While,

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
    pub fn new(lexeme: String, literal: Option<Literal>, line: usize) -> Self {
        Token {
            kind: lexeme.as_str().into(),
            lexeme,
            literal,
            line,
        }
    }
}

impl From<&str> for TokenType {
    fn from(lexeme: &str) -> Self {
        match lexeme {
            "(" => TokenType::LeftParen,
            ")" => TokenType::RightParen,
            "{" => TokenType::LeftBrace,
            "}" => TokenType::RightBrace,
            "," => TokenType::Comma,
            "." => TokenType::Dot,
            ".." => TokenType::DotDot,
            "-" => TokenType::Minus,
            "+" => TokenType::Plus,
            "%" => TokenType::Modulo,
            ":" => TokenType::Colon,
            ";" => TokenType::Semicolon,
            "/" => TokenType::Slash,
            "*" => TokenType::Star,
            "!" => TokenType::Bang,
            "!=" => TokenType::BangEqual,
            "=" => TokenType::Equal,
            "==" => TokenType::EqualEqual,
            ">" => TokenType::Greater,
            ">=" => TokenType::GreaterEqual,
            "<" => TokenType::Less,
            "<=" => TokenType::LessEqual,
            "and" => TokenType::And,
            "struct" => TokenType::Struct,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "fn" => TokenType::Fn,
            "for" => TokenType::For,
            "if" => TokenType::If,
            "in" => TokenType::In,
            "none" => TokenType::None,
            "or" => TokenType::Or,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "while" => TokenType::While,
            "" => TokenType::Eof,
            string if string.starts_with('"') => TokenType::String,
            num if num.parse::<f64>().is_ok() => TokenType::Number,
            _ => TokenType::Identifier,
        }
    }
}
