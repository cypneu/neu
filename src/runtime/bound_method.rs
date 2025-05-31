use std::rc::Rc;

use crate::runtime::callable::Callable;
use crate::runtime::callable::CallableObj;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

pub struct BoundMethod {
    receiver: Value,
    method: CallableObj,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: CallableObj) -> Rc<Self> {
        Rc::new(Self { receiver, method })
    }
}

impl Callable for BoundMethod {
    fn arity(&self) -> Option<usize> {
        self.method.arity().map(|n| n.saturating_sub(1))
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        mut args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        args.insert(0, self.receiver.clone());
        self.method.call(interpreter, args)
    }
}
