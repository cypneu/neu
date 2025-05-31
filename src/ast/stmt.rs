use std::rc::Rc;

use crate::ast::expr::Expr;
use crate::frontend::token::Token;

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct StructDecl {
    pub name: Token,
    pub fields: Vec<Token>,
    pub methods: Vec<Rc<FunctionDecl>>,
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
    FunctionDecl(Rc<FunctionDecl>),
    Return {
        value: Option<Expr>,
    },
    StructDecl(StructDecl),
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
    fn visit_return_stmt(&mut self, value: &Option<Expr>) -> T;
    fn visit_struct_declaration(&mut self, struct_decl: &StructDecl) -> T;
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
            Stmt::FunctionDecl(func_decl) => visitor.visit_func_declaration(func_decl),
            Stmt::Return { value } => visitor.visit_return_stmt(value),
            Stmt::StructDecl(struct_decl) => visitor.visit_struct_declaration(struct_decl),
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

    pub fn func_declaration(func_decl: FunctionDecl) -> Self {
        Stmt::FunctionDecl(Rc::new(func_decl))
    }

    pub fn struct_declaration(
        name: Token,
        fields: Vec<Token>,
        methods: Vec<Rc<FunctionDecl>>,
    ) -> Self {
        Stmt::StructDecl(StructDecl {
            name,
            fields,
            methods,
        })
    }
}
