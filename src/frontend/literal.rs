#[derive(Clone, Debug)]
pub enum Literal {
    None,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl Literal {
    pub fn as_number(&self) -> Option<f64> {
        if let Literal::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        if let Literal::String(s) = self {
            Some(s)
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Literal::Boolean(b) = self {
            Some(*b)
        } else {
            None
        }
    }
}

impl From<f64> for Literal {
    fn from(n: f64) -> Self {
        Literal::Number(n)
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        Literal::Boolean(b)
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::String(s)
    }
}

impl<'a> From<&'a str> for Literal {
    fn from(s: &'a str) -> Self {
        Literal::String(s.to_owned())
    }
}
