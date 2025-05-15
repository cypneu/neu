use crate::runtime::callable::Callable;
use crate::runtime::environment::Environment;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct Clock;

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> Result<Value, RuntimeError> {
        let since_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("System clock is before UNIX_EPOCH!");

        let seconds = since_epoch.as_secs_f64();

        Ok(Value::Number(seconds))
    }
}

pub fn register_natives(env: &mut Environment) {
    let clock: Rc<dyn Callable> = Rc::new(Clock);
    env.assign("clock", Value::Callable(clock));
}
