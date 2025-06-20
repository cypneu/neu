use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::frontend::literal::Literal;
use crate::runtime::callable::CallableObj;

use crate::runtime::struct_declaration::StructDeclaration;
use crate::runtime::struct_instance::StructInstance;

#[derive(Clone)]
pub enum Value<'src> {
    None,
    Boolean(bool),
    String(String),
    Number(f64),
    Callable(CallableObj<'src>),
    StructDecl(StructDeclaration<'src>),
    StructInstance(Rc<RefCell<StructInstance<'src>>>),
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::None => write!(f, "None"),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Number(n) => write!(f, "Number({})", n),
            Value::Callable(_) => write!(f, "Callable(<fn>)"),
            Value::StructInstance(rc) => write!(f, "struct {}{{}}", rc.borrow()),
            Value::StructDecl(s) => write!(f, "{} initializer", s.name),
        }
    }
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => write!(f, "none"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Callable(_) => write!(f, "<fn>"),
            Value::StructInstance(rc) => write!(f, "{}", rc.borrow()),
            Value::StructDecl(s) => write!(f, "{} initializer", s.name),
        }
    }
}

impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::None, Value::None) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Callable(a), Value::Callable(b)) => Rc::ptr_eq(a, b),
            (Value::StructInstance(a), Value::StructInstance(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl<'src> From<Literal<'src>> for Value<'src> {
    fn from(lit: Literal<'src>) -> Self {
        match lit {
            Literal::None => Value::None,
            Literal::Boolean(b) => Value::Boolean(b),
            Literal::String(s) => Value::String(s.to_string()),
            Literal::Number(n) => Value::Number(n),
        }
    }
}

impl Value<'_> {
    pub fn as_number(&self) -> Option<f64> {
        if let Value::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Value::Number(n) if n.fract() == 0.0 => Some(*n as i64),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Value::Boolean(b) = self {
            Some(*b)
        } else {
            None
        }
    }
}

impl From<f64> for Value<'_> {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl From<bool> for Value<'_> {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl From<String> for Value<'_> {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl<'a> From<&'a str> for Value<'_> {
    fn from(s: &'a str) -> Self {
        Value::String(s.to_owned())
    }
}
