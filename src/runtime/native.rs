use crate::runtime::callable::Callable;
use crate::runtime::environment::Environment;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct Clock;

impl Callable for Clock {
    fn arity(&self) -> Option<usize> {
        Some(0)
    }

    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> Result<Value, RuntimeError> {
        let since_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("System clock is before UNIX_EPOCH!");

        let seconds = since_epoch.as_secs_f64();

        Ok(Value::Number(seconds))
    }
}

pub struct Print;

impl Callable for Print {
    fn arity(&self) -> Option<usize> {
        None
    }

    fn call(&self, _: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError> {
        use std::io::{self, Write};
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        for (i, val) in args.iter().enumerate() {
            if i > 0 {
                write!(handle, " ").unwrap();
            }
            write!(handle, "{}", val).unwrap();
        }
        writeln!(handle).unwrap();
        Ok(Value::None)
    }
}

pub fn register_natives(env: &mut Environment) {
    let clock = Rc::new(Clock);
    env.define("clock", Value::Callable(clock));

    let print_fn = Rc::new(Print);
    env.define("print", Value::Callable(print_fn));
}
