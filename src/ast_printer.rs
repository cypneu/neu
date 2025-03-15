use crate::expr::{Expr, Value};
use crate::token::{Literal, Token};
use crate::visitor::Visitor;

#[derive(Debug)]
pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }

    fn parenthesize(&mut self, name: &str, exprs: &[&Expr]) -> String {
        let mut builder = String::new();
        builder.push('(');
        builder.push_str(name);
        for expr in exprs {
            builder.push(' ');
            builder.push_str(&expr.accept(self));
        }
        builder.push(')');
        builder
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> String {
        self.parenthesize(&operator.lexeme, &[left, right])
    }
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> String {
        self.parenthesize(&operator.lexeme, &[right])
    }
    fn visit_literal_expr(&mut self, value: &Value) -> String {
        match value {
            Value::Literal(Literal::Number(number)) => number.to_string(),
            Value::Literal(Literal::String(string)) => string.into(),
            value => format!("{:?}", value),
        }
    }
    fn visit_grouping_expr(&mut self, expr: &Expr) -> String {
        self.parenthesize("group", &[expr])
    }
}
