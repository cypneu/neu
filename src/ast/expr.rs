use crate::frontend::literal::Literal;
use crate::frontend::token::Token;

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
    Literal(Literal),
    Grouping {
        expression: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

pub trait Visitor<T> {
    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> T;
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> T;
    fn visit_literal_expr(&mut self, value: &Literal) -> T;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> T;
    fn visit_variable_expr(&mut self, name: &Token) -> T;
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
            Expr::Variable { name } => visitor.visit_variable_expr(name),
        }
    }
}
