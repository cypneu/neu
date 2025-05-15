use crate::frontend::token::Token;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::collections::HashMap;

pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new(enclosing: Option<Box<Environment>>) -> Self {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn assign(&mut self, name: &str, val: Value) {
        if self.assign_to_enclosing(name, &val) {
            return;
        }

        self.values.insert(name.to_string(), val);
    }

    pub fn get(&self, name: &Token) -> Result<&Value, RuntimeError> {
        if let Some(v) = self.values.get(&name.lexeme) {
            Ok(v)
        } else if let Some(ref parent) = self.enclosing {
            parent.get(name)
        } else {
            let msg = format!("Undefined variable '{}'", name.lexeme);
            Err(RuntimeError::with_token(name, &msg))
        }
    }

    fn assign_to_enclosing(&mut self, name: &str, val: &Value) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), val.clone());
            true
        } else if let Some(parent) = self.enclosing.as_mut() {
            parent.assign_to_enclosing(name, val)
        } else {
            false
        }
    }
}
