use std::rc::Rc;

use crate::ast::expr::Expr;
use crate::frontend::token::Token;

#[derive(Debug)]
pub struct FunctionDecl<'src> {
    pub name: Token<'src>,
    pub params: Vec<Token<'src>>,
    pub body: Vec<Stmt<'src>>,
}

#[derive(Debug)]
pub struct StructDecl<'src> {
    pub name: Token<'src>,
    pub fields: Vec<Token<'src>>,
    pub methods: Vec<Rc<FunctionDecl<'src>>>,
}

#[derive(Debug)]
pub enum Stmt<'src> {
    Expr(Expr<'src>),
    Block(Vec<Stmt<'src>>),
    If {
        condition: Expr<'src>,
        then_branch: Box<Stmt<'src>>,
        else_branch: Option<Box<Stmt<'src>>>,
    },
    While {
        condition: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
    For {
        var: Token<'src>,
        start: Expr<'src>,
        end: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
    FunctionDecl(Rc<FunctionDecl<'src>>),
    Return {
        value: Option<Expr<'src>>,
    },
    StructDecl(StructDecl<'src>),
    Break,
    Continue,
}

pub trait Visitor<'src, T> {
    fn visit_expression_stmt(&mut self, expr: &Expr<'src>) -> T;
    fn visit_block_stmt(&mut self, stmts: &[Stmt<'src>]) -> T;
    fn visit_if_stmt(
        &mut self,
        condition: &Expr<'src>,
        then_branch: &Stmt<'src>,
        else_branch: Option<&Stmt<'src>>,
    ) -> T;
    fn visit_while_stmt(&mut self, condition: &Expr<'src>, body: &Stmt<'src>) -> T;
    fn visit_for_stmt(
        &mut self,
        var: &Token<'src>,
        start: &Expr<'src>,
        end: &Expr<'src>,
        body: &Stmt<'src>,
    ) -> T;
    fn visit_func_declaration(&mut self, func_decl: &Rc<FunctionDecl<'src>>) -> T;
    fn visit_return_stmt(&mut self, value: &Option<Expr<'src>>) -> T;
    fn visit_struct_declaration(&mut self, struct_decl: &StructDecl<'src>) -> T;
    fn visit_break_stmt(&mut self) -> T;
    fn visit_continue_stmt(&mut self) -> T;
}

impl<'src> Stmt<'src> {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<'src, T>) -> T {
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
            Stmt::For {
                var,
                start,
                end,
                body,
            } => visitor.visit_for_stmt(var, start, end, body),
            Stmt::FunctionDecl(func_decl) => visitor.visit_func_declaration(func_decl),
            Stmt::Return { value } => visitor.visit_return_stmt(value),
            Stmt::StructDecl(struct_decl) => visitor.visit_struct_declaration(struct_decl),
            Stmt::Break => visitor.visit_break_stmt(),
            Stmt::Continue => visitor.visit_continue_stmt(),
        }
    }

    pub fn if_stmt(
        condition: Expr<'src>,
        then_branch: Stmt<'src>,
        else_branch: Option<Stmt<'src>>,
    ) -> Self {
        Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }
    }

    pub fn while_stmt(condition: Expr<'src>, body: Stmt<'src>) -> Self {
        Stmt::While {
            condition,
            body: Box::new(body),
        }
    }

    pub fn func_declaration(func_decl: FunctionDecl<'src>) -> Self {
        Stmt::FunctionDecl(Rc::new(func_decl))
    }

    pub fn struct_declaration(
        name: Token<'src>,
        fields: Vec<Token<'src>>,
        methods: Vec<Rc<FunctionDecl<'src>>>,
    ) -> Self {
        Stmt::StructDecl(StructDecl {
            name,
            fields,
            methods,
        })
    }

    pub fn for_stmt(
        var: Token<'src>,
        start: Expr<'src>,
        end: Expr<'src>,
        body: Stmt<'src>,
    ) -> Self {
        Stmt::For {
            var,
            start,
            end,
            body: Box::new(body),
        }
    }
}
