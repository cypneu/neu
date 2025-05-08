use crate::frontend::token::Token;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn assign(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    pub fn get(&self, name: &Token) -> Result<&Value, RuntimeError> {
        self.values.get(&name.lexeme).ok_or_else(|| {
            let message = format!("Undefined variable '{}'.", name.lexeme);
            RuntimeError::new(name, &message)
        })
    }
}
