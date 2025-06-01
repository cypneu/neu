use std::collections::HashMap;

use crate::frontend::token::Token;
use crate::runtime::interpreter::EnvRef;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

pub struct Environment {
    pub enclosing: Option<EnvRef>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new(enclosing: Option<EnvRef>) -> Self {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, val: Value) {
        self.values.insert(name.to_string(), val);
    }

    pub fn assign(&mut self, name: &str, val: Value) {
        let rc_val = val;
        if self.assign_to_enclosing(name, &rc_val) {
            return;
        }

        self.values.insert(name.to_string(), rc_val);
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        if let Some(v) = self.values.get(&name.lexeme) {
            Ok(v.clone())
        } else if let Some(parent) = &self.enclosing {
            parent.borrow().get(name)
        } else {
            let msg = format!("Undefined variable '{}'", name.lexeme);
            Err(RuntimeError::with_token(name, &msg))
        }
    }

    fn assign_to_enclosing(&mut self, name: &str, val: &Value) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), val.clone());
            true
        } else if let Some(parent) = &self.enclosing {
            parent.borrow_mut().assign_to_enclosing(name, val)
        } else {
            false
        }
    }
}
