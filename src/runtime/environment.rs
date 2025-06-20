use std::collections::HashMap;

use crate::frontend::token::Token;
use crate::runtime::interpreter::EnvRef;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

use std::rc::Rc;

pub struct Environment<'src> {
    pub enclosing: Option<EnvRef<'src>>,
    values: HashMap<&'src str, Rc<Value<'src>>>,
}

impl<'src> Environment<'src> {
    pub fn new(enclosing: Option<EnvRef<'src>>) -> Self {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &'src str, val: Rc<Value<'src>>) {
        self.values.insert(name, val);
    }

    pub fn assign(&mut self, name: &'src str, val: Rc<Value<'src>>) {
        if self.assign_to_enclosing(name, &val) {
            return;
        }

        self.values.insert(name, val);
    }

    pub fn get(&self, name: &Token<'src>) -> Result<Rc<Value<'src>>, RuntimeError<'src>> {
        if let Some(v) = self.values.get(name.lexeme) {
            Ok(v.clone())
        } else if let Some(parent) = &self.enclosing {
            parent.borrow().get(name)
        } else {
            let msg = format!("Undefined variable '{}'", name.lexeme);
            Err(RuntimeError::with_token(name, &msg))
        }
    }

    fn assign_to_enclosing(&mut self, name: &'src str, val: &Rc<Value<'src>>) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name, val.clone());
            true
        } else if let Some(parent) = &self.enclosing {
            parent.borrow_mut().assign_to_enclosing(name, val)
        } else {
            false
        }
    }
}
