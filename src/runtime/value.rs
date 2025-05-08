use crate::frontend::literal::Literal;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    None,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl From<Literal> for Value {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::None => Value::None,
            Literal::Boolean(b) => Value::Boolean(b),
            Literal::String(s) => Value::String(s),
            Literal::Number(n) => Value::Number(n),
        }
    }
}

impl Value {
    pub fn as_number(&self) -> Option<f64> {
        if let Value::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        if let Value::String(s) = self {
            Some(s)
        } else {
            None
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

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Boolean(b)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl<'a> From<&'a str> for Value {
    fn from(s: &'a str) -> Self {
        Value::String(s.to_owned())
    }
}
