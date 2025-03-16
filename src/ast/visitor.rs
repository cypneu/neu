use crate::ast::expr::Expr;
use crate::frontend::literal::Literal;
use crate::frontend::token::Token;

pub trait Visitor<T> {
    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> T;
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> T;
    fn visit_literal_expr(&mut self, value: &Literal) -> T;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> T;
}
