use crate::frontend::literal::Literal;
use crate::frontend::token::Token;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Binary {
        left: Box<Expr<'src>>,
        op: Token<'src>,
        right: Box<Expr<'src>>,
    },
    Unary {
        op: Token<'src>,
        right: Box<Expr<'src>>,
    },
    Literal(Literal<'src>),
    Grouping {
        expression: Box<Expr<'src>>,
    },
    Variable {
        name: Token<'src>,
    },
    Assignment {
        name: Token<'src>,
        value: Box<Expr<'src>>,
    },
    Logical {
        left: Box<Expr<'src>>,
        op: Token<'src>,
        right: Box<Expr<'src>>,
    },
    Call {
        callee: Box<Expr<'src>>,
        arguments: Vec<Expr<'src>>,
    },
    StructInit {
        initializer: Box<Expr<'src>>,
        fields: Vec<(Token<'src>, Expr<'src>)>,
    },
    Get {
        name: Token<'src>,
        expr: Box<Expr<'src>>,
    },
    Set {
        name: Token<'src>,
        expr: Box<Expr<'src>>,
        value: Box<Expr<'src>>,
    },
}

pub trait Visitor<'src, T> {
    fn visit_binary_expr(
        &mut self,
        left: &Expr<'src>,
        operator: &Token<'src>,
        right: &Expr<'src>,
    ) -> T;
    fn visit_unary_expr(&mut self, operator: &Token<'src>, right: &Expr<'src>) -> T;
    fn visit_literal_expr(&mut self, value: &Literal<'src>) -> T;
    fn visit_grouping_expr(&mut self, expr: &Expr<'src>) -> T;
    fn visit_variable_expr(&mut self, name: &Token<'src>) -> T;
    fn visit_assignment_expr(&mut self, name: &Token<'src>, expr: &Expr<'src>) -> T;
    fn visit_logical_expr(&mut self, left: &Expr<'src>, op: &Token<'src>, right: &Expr<'src>) -> T;
    fn visit_call_expr(&mut self, callee: &Expr<'src>, arguments: &[Expr<'src>]) -> T;
    fn visit_struct_init_expr(
        &mut self,
        initializer: &Expr<'src>,
        fields: &[(Token<'src>, Expr<'src>)],
    ) -> T;
    fn visit_get_expr(&mut self, name: &Token<'src>, expr: &Expr<'src>) -> T;
    fn visit_set_expr(&mut self, name: &Token<'src>, expr: &Expr<'src>, value: &Expr<'src>) -> T;
}

impl<'src> Expr<'src> {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<'src, T>) -> T {
        match self {
            Expr::Binary { left, op, right } => visitor.visit_binary_expr(left, op, right),
            Expr::Unary { op, right } => visitor.visit_unary_expr(op, right),
            Expr::Literal(value) => visitor.visit_literal_expr(value),
            Expr::Grouping { expression } => visitor.visit_grouping_expr(expression),
            Expr::Variable { name } => visitor.visit_variable_expr(name),
            Expr::Assignment { name, value } => visitor.visit_assignment_expr(name, value),
            Expr::Logical { left, op, right } => visitor.visit_logical_expr(left, op, right),
            Expr::Call { callee, arguments } => visitor.visit_call_expr(callee, arguments),
            Expr::StructInit {
                initializer,
                fields,
            } => visitor.visit_struct_init_expr(initializer, fields),
            Expr::Get { name, expr } => visitor.visit_get_expr(name, expr),
            Expr::Set { name, expr, value } => visitor.visit_set_expr(name, expr, value),
        }
    }

    pub fn group(inner: Expr<'src>) -> Self {
        let expression = Box::new(inner);
        Expr::Grouping { expression }
    }

    pub fn unary(op: Token<'src>, right: Expr<'src>) -> Self {
        let right = Box::new(right);
        Expr::Unary { op, right }
    }

    pub fn binary(left: Expr<'src>, op: Token<'src>, right: Expr<'src>) -> Self {
        let (left, right) = (Box::new(left), Box::new(right));
        Expr::Binary { left, op, right }
    }

    pub fn variable(name: Token<'src>) -> Self {
        Expr::Variable { name }
    }

    pub fn assign(name: Token<'src>, value: Expr<'src>) -> Self {
        let value = Box::new(value);
        Expr::Assignment { name, value }
    }

    pub fn logical(left: Expr<'src>, op: Token<'src>, right: Expr<'src>) -> Self {
        let (left, right) = (Box::new(left), Box::new(right));
        Expr::Logical { left, op, right }
    }

    pub fn call(callee: Expr<'src>, arguments: Vec<Expr<'src>>) -> Self {
        let callee = Box::new(callee);
        Expr::Call { callee, arguments }
    }

    pub fn struct_init(initializer: Expr<'src>, fields: Vec<(Token<'src>, Expr<'src>)>) -> Self {
        Expr::StructInit {
            initializer: Box::new(initializer),
            fields,
        }
    }

    pub fn get(expr: Expr<'src>, name: Token<'src>) -> Self {
        let expr = Box::new(expr);
        Expr::Get { name, expr }
    }

    pub fn set(expr: Box<Expr<'src>>, name: Token<'src>, value: Expr<'src>) -> Self {
        Expr::Set {
            name,
            expr,
            value: Box::new(value),
        }
    }
}

impl<'src> From<Literal<'src>> for Expr<'src> {
    fn from(lit: Literal<'src>) -> Expr<'src> {
        Expr::Literal(lit)
    }
}

impl<'src> From<bool> for Expr<'src> {
    fn from(b: bool) -> Expr<'src> {
        Expr::Literal(Literal::Boolean(b))
    }
}
