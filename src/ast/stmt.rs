use crate::ast::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
}

pub trait Visitor<T> {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Stmt::Expr(expr) => visitor.visit_expression_stmt(expr),
        }
    }
}
