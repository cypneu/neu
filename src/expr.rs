use crate::token::{Literal, Token};
use crate::visitor::Visitor;

#[derive(Debug)]
pub enum Value {
    None,
    True,
    False,
    Literal(Literal),
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal(Value),
    Grouping {
        expression: Box<Expr>,
    },
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => visitor.visit_binary_expr(left, operator, right),
            Expr::Unary { operator, right } => visitor.visit_unary_expr(operator, right),
            Expr::Literal(value) => visitor.visit_literal_expr(value),
            Expr::Grouping { expression } => visitor.visit_grouping_expr(expression),
        }
    }
}
