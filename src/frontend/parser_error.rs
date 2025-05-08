use crate::frontend::token::Token;

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
