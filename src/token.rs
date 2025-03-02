#[derive(Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Modulo,
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
    Class,
    Else,
    False,
    Fn,
    For,
    If,
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

#[derive(Debug)]
pub enum LiteralValue {
    None,
    String(String),
    Number(f64),
}

#[derive(Debug)]
pub struct Token {
    kind: TokenType,
    pub lexeme: String,
    literal_value: LiteralValue,
    line: usize,
}

impl Token {
    pub fn new(lexeme: String, literal_value: LiteralValue, line: usize) -> Self {
        Token {
            kind: lexeme.as_str().into(),
            lexeme,
            literal_value,
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
            "-" => TokenType::Minus,
            "+" => TokenType::Plus,
            "%" => TokenType::Modulo,
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
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "fn" => TokenType::Fn,
            "for" => TokenType::For,
            "if" => TokenType::If,
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
