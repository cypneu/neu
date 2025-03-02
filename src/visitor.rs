use crate::expr::Expr;
use crate::token::{LiteralValue, Token};

pub trait Visitor<T> {
    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> T;
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> T;
    fn visit_literal_expr(&mut self, value: &LiteralValue) -> T;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> T;
}
