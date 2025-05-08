use crate::frontend::token::Token;

#[derive(Debug)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

impl RuntimeError {
    pub fn new(token: &Token, message: &str) -> Self {
        Self {
            message: message.to_string(),
            token: token.clone(),
        }
    }
}
