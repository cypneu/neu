use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::rc::Rc;

pub trait Callable<'src> {
    fn arity(&self) -> Option<usize>;
    fn call(
        &self,
        interpreter: &mut Interpreter<'src>,
        args: Vec<Rc<Value<'src>>>,
    ) -> Result<Rc<Value<'src>>, RuntimeError<'src>>;
}

pub type CallableObj<'src> = Rc<dyn Callable<'src> + 'src>;
