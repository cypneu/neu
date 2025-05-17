use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::rc::Rc;

pub trait Callable {
    fn arity(&self) -> Option<usize>;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError>;
}

pub type CallableObj = Rc<dyn Callable>;
