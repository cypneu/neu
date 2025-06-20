use std::rc::Rc;

use crate::runtime::callable::Callable;
use crate::runtime::callable::CallableObj;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

pub struct BoundMethod<'src> {
    receiver: Rc<Value<'src>>,
    method: CallableObj<'src>,
}

impl<'src> BoundMethod<'src> {
    pub fn new(receiver: Rc<Value<'src>>, method: CallableObj<'src>) -> Rc<Self> {
        Rc::new(Self { receiver, method })
    }
}

impl<'src> Callable<'src> for BoundMethod<'src> {
    fn arity(&self) -> Option<usize> {
        self.method.arity().map(|n| n.saturating_sub(1))
    }

    fn call(
        &self,
        interpreter: &mut Interpreter<'src>,
        mut args: Vec<Rc<Value<'src>>>,
    ) -> Result<Rc<Value<'src>>, RuntimeError<'src>> {
        args.insert(0, Rc::clone(&self.receiver));
        self.method.call(interpreter, args)
    }
}
