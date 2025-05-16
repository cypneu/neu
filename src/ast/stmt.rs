use crate::ast::expr::Expr;
use crate::frontend::token::Token;
use std::rc::Rc;

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function(Rc<FunctionDecl>),
}

pub trait Visitor<T> {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> T;
    fn visit_block_stmt(&mut self, stmts: &[Stmt]) -> T;
    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> T;
    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> T;
    fn visit_func_declaration(&mut self, func_decl: &Rc<FunctionDecl>) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Stmt::Expr(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Block(stmts) => visitor.visit_block_stmt(stmts),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let else_ref: Option<&Stmt> = else_branch.as_deref();
                visitor.visit_if_stmt(condition, then_branch, else_ref)
            }
            Stmt::While { condition, body } => visitor.visit_while_stmt(condition, body),
            Stmt::Function(func_decl) => visitor.visit_func_declaration(func_decl),
        }
    }

    pub fn if_stmt(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }
    }

    pub fn while_stmt(condition: Expr, body: Stmt) -> Self {
        Stmt::While {
            condition,
            body: Box::new(body),
        }
    }

    pub fn func_declaration(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Stmt::Function(Rc::new(FunctionDecl { name, params, body }))
    }
}
