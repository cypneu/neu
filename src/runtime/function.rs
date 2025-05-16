use crate::ast::stmt::FunctionDecl;
use crate::runtime::callable::Callable;
use crate::runtime::environment::Environment;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::rc::Rc;

pub struct Function {
    pub declaration: Rc<FunctionDecl>,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let globals_ref = Rc::clone(&interpreter.globals);
        let mut environment = Environment::new(Some(globals_ref));

        for (param, arg) in self.declaration.params.iter().zip(arguments.into_iter()) {
            environment.assign(&param.lexeme, arg);
        }

        interpreter.execute_block(&self.declaration.body, environment)?;
        Ok(Value::None)
    }
}
