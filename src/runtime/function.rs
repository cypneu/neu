use crate::ast::stmt::FunctionDecl;
use crate::runtime::callable::Callable;
use crate::runtime::environment::Environment;
use crate::runtime::interpreter::EnvRef;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::ops::ControlFlow;
use std::rc::Rc;

pub struct Function {
    pub declaration: Rc<FunctionDecl>,
    pub closure: EnvRef,
}

impl Callable for Function {
    fn arity(&self) -> Option<usize> {
        Some(self.declaration.params.len())
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let closure_ref = Rc::clone(&self.closure);
        let mut environment = Environment::new(Some(closure_ref));

        for (param, arg) in self.declaration.params.iter().zip(arguments.into_iter()) {
            environment.assign(&param.lexeme, arg);
        }

        interpreter
            .execute_block(&self.declaration.body, environment)
            .map(|cf| match cf {
                ControlFlow::Continue(_) => Value::None,
                ControlFlow::Break(value) => value,
            })
    }
}
