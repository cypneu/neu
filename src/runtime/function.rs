use crate::ast::stmt::FunctionDecl;
use crate::runtime::callable::Callable;
use crate::runtime::environment::Environment;
use crate::runtime::interpreter::EnvRef;
use crate::runtime::interpreter::ExecutionFlow;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

use std::rc::Rc;

pub struct Function<'src> {
    pub declaration: Rc<FunctionDecl<'src>>,
    pub closure: EnvRef<'src>,
}

impl<'src> Callable<'src> for Function<'src> {
    fn arity(&self) -> Option<usize> {
        Some(self.declaration.params.len())
    }

    fn call(
        &self,
        interpreter: &mut Interpreter<'src>,
        arguments: Vec<Rc<Value<'src>>>,
    ) -> Result<Rc<Value<'src>>, RuntimeError<'src>> {
        let closure_ref = Rc::clone(&self.closure);
        let mut environment = Environment::new(Some(closure_ref));

        for (param, arg) in self.declaration.params.iter().zip(arguments.into_iter()) {
            environment.define(param.lexeme, arg);
        }

        interpreter
            .execute_callable_body(&self.declaration.body, environment)
            .map(|cf| match cf {
                ExecutionFlow::Return(value) => value,
                _ => Rc::new(Value::None),
            })
    }
}
