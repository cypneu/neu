use crate::ast::expr::{Expr, Visitor};
use crate::frontend::literal::Literal;
use crate::frontend::token::Token;

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

    fn visit_literal_expr(&mut self, literal: &Literal) -> String {
        match literal {
            Literal::Number(number) => number.to_string(),
            Literal::String(string) => string.into(),
            value => format!("{:?}", value),
        }
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> String {
        self.parenthesize("group", &[expr])
    }

    fn visit_variable_expr(&mut self, name: &Token) -> String {
        format!("Variable: {:?}", name)
    }

    fn visit_assignment_expr(&mut self, name: &Token, value: &Expr) -> String {
        format!("Name: {:?}, Value: {:?}", name, value)
    }

    fn visit_logical_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> String {
        format!("Left: {:?}, Op: {:?}, Right: {:?}", left, op, right)
    }

    fn visit_call_expr(&mut self, callee: &Expr, arguments: &[Expr]) -> String {
        format!("Callee: {:?}, arguments: {:?}", callee, arguments)
    }

    fn visit_struct_init_expr(&mut self, _initializer: &Expr, fields: &[(Token, Expr)]) -> String {
        format!("Struct init with fields: {:?}", fields)
    }

    fn visit_get_expr(&mut self, name: &Token, expr: &Expr) -> String {
        format!("Get property: {:?}, struct: {:?}", name, expr)
    }

    fn visit_set_expr(&mut self, name: &Token, expr: &Expr, value: &Expr) -> String {
        format!(
            "Set property: {:?}, struct: {:?}, value: {:?}",
            name, expr, value
        )
    }
}
