use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::frontend::token::Token;
use crate::runtime::struct_declaration::StructDeclaration;
use crate::runtime::value::Value;

use super::runtime_error::RuntimeError;

#[derive(Clone)]
pub struct StructInstance<'src> {
    pub struct_decl: StructDeclaration<'src>,
    fields: HashMap<&'src str, Rc<Value<'src>>>,
}

impl<'src> StructInstance<'src> {
    pub fn new(struct_decl: StructDeclaration<'src>) -> Self {
        StructInstance {
            struct_decl,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &Token<'src>) -> Result<Rc<Value<'src>>, RuntimeError<'src>> {
        if let Some(value) = self.fields.get(name.lexeme) {
            return Ok(Rc::clone(value));
        }

        if let Some(method) = self.struct_decl.methods.get(name.lexeme) {
            return Ok(Rc::new(Value::Callable(Rc::clone(method))));
        }

        let msg = format!("Undefined property '{}'.", name.lexeme);
        Err(RuntimeError::with_token(name, &msg))
    }

    pub fn set(
        &mut self,
        name: &Token<'src>,
        value: Rc<Value<'src>>,
    ) -> Result<(), RuntimeError<'src>> {
        if !self.struct_decl.fields.contains(name.lexeme) {
            let msg = format!(
                "'{}' is not a field of struct {}",
                name.lexeme, self.struct_decl.name
            );
            return Err(RuntimeError::with_token(name, &msg));
        }
        self.fields.insert(name.lexeme, value);
        Ok(())
    }
}

impl fmt::Display for StructInstance<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.struct_decl.name)
    }
}
