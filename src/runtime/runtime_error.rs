use crate::frontend::token::Token;

#[derive(Debug)]
pub struct RuntimeError {
    pub token: Option<Token>,
    pub message: String,
}

impl RuntimeError {
    pub fn with_token(token: &Token, message: &str) -> Self {
        Self {
            message: message.to_string(),
            token: Some(token.clone()),
        }
    }

    pub fn new(message: &str) -> Self {
        Self {
            message: message.to_string(),
            token: None,
        }
    }
}
