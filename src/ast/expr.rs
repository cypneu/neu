use crate::frontend::literal::Literal;
use crate::frontend::token::Token;

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    Literal(Literal),
    Grouping {
        expression: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Assignment {
        name: Token,
        value: Box<Expr>,
    },
}

pub trait Visitor<T> {
    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> T;
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> T;
    fn visit_literal_expr(&mut self, value: &Literal) -> T;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> T;
    fn visit_variable_expr(&mut self, name: &Token) -> T;
    fn visit_assignment_expr(&mut self, name: &Token, expr: &Expr) -> T;
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Expr::Binary { left, op, right } => visitor.visit_binary_expr(left, op, right),
            Expr::Unary { op, right } => visitor.visit_unary_expr(op, right),
            Expr::Literal(value) => visitor.visit_literal_expr(value),
            Expr::Grouping { expression } => visitor.visit_grouping_expr(expression),
            Expr::Variable { name } => visitor.visit_variable_expr(name),
            Expr::Assignment { name, value } => visitor.visit_assignment_expr(name, value),
        }
    }

    pub fn group(inner: Expr) -> Self {
        let expression = Box::new(inner);
        Expr::Grouping { expression }
    }

    pub fn unary(op: Token, right: Expr) -> Self {
        let right = Box::new(right);
        Expr::Unary { op, right }
    }

    pub fn binary(left: Expr, op: Token, right: Expr) -> Self {
        let (left, right) = (Box::new(left), Box::new(right));
        Expr::Binary { left, op, right }
    }

    pub fn variable(name: Token) -> Self {
        Expr::Variable { name }
    }

    pub fn assign(name: Token, value: Expr) -> Self {
        let value = Box::new(value);
        Expr::Assignment { name, value }
    }
}

impl From<Literal> for Expr {
    fn from(lit: Literal) -> Expr {
        Expr::Literal(lit)
    }
}

impl From<bool> for Expr {
    fn from(b: bool) -> Expr {
        Expr::Literal(Literal::Boolean(b))
    }
}
