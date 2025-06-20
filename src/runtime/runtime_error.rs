use crate::frontend::token::Token;

#[derive(Debug)]
pub struct RuntimeError<'src> {
    pub token: Option<Token<'src>>,
    pub message: String,
}

impl<'src> RuntimeError<'src> {
    pub fn with_token(token: &Token<'src>, message: &str) -> Self {
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
