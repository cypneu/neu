use crate::ast::expr::Expr;
use crate::frontend::token::Token;

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Variable {
        name: Token,
        initializer: Option<Expr>,
    },
}

pub trait Visitor<T> {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> T;
    fn visit_variable_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Stmt::Expr(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Variable { name, initializer } => visitor.visit_variable_stmt(name, initializer),
        }
    }
}
