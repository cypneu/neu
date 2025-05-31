use std::collections::HashMap;
use std::rc::Rc;

use crate::frontend::token::Token;
use crate::runtime::interpreter::EnvRef;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

pub struct Environment {
    pub enclosing: Option<EnvRef>,
    values: HashMap<String, Rc<Value>>,
}

impl Environment {
    pub fn new(enclosing: Option<EnvRef>) -> Self {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, val: Value) {
        self.values.insert(name.to_string(), Rc::new(val));
    }

    pub fn assign(&mut self, name: &str, val: Value) {
        let rc_val = Rc::new(val);
        if self.assign_to_enclosing(name, &rc_val) {
            return;
        }

        self.values.insert(name.to_string(), rc_val);
    }

    pub fn get(&self, name: &Token) -> Result<Rc<Value>, RuntimeError> {
        if let Some(v) = self.values.get(&name.lexeme) {
            Ok(Rc::clone(v))
        } else if let Some(parent) = &self.enclosing {
            parent.borrow().get(name)
        } else {
            let msg = format!("Undefined variable '{}'", name.lexeme);
            Err(RuntimeError::with_token(name, &msg))
        }
    }

    fn assign_to_enclosing(&mut self, name: &str, val: &Rc<Value>) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), Rc::clone(val));
            true
        } else if let Some(parent) = &self.enclosing {
            parent.borrow_mut().assign_to_enclosing(name, val)
        } else {
            false
        }
    }
}
