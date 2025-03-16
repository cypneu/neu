use crate::ast::expr::Expr;
use crate::ast::visitor::Visitor;
use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};

pub struct Interpreter;

impl Visitor<Literal> for Interpreter {
    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Literal {
        let left_val = left.accept(self);
        let right_val = right.accept(self);

        use TokenType::*;
        match operator.kind {
            Plus => self.eval_plus(left_val, right_val),
            Minus => self.eval_numeric_binop(left_val, right_val, |a, b| a - b),
            Slash => self.eval_numeric_binop(left_val, right_val, |a, b| a / b),
            Star => self.eval_numeric_binop(left_val, right_val, |a, b| a * b),
            Modulo => self.eval_numeric_binop(left_val, right_val, |a, b| a % b),

            Greater => self.eval_numeric_binop(left_val, right_val, |a, b| a > b),
            GreaterEqual => self.eval_numeric_binop(left_val, right_val, |a, b| a >= b),
            Less => self.eval_numeric_binop(left_val, right_val, |a, b| a < b),
            LessEqual => self.eval_numeric_binop(left_val, right_val, |a, b| a <= b),

            BangEqual => Literal::from(!(left_val == right_val)),
            EqualEqual => Literal::from(left_val == right_val),

            _ => unreachable!("Unknown binary operator"),
        }
    }

    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> Literal {
        let val = right.accept(self);

        use TokenType::*;
        match operator.kind {
            Minus => (-val.as_number().expect("Expected a number for unary minus")).into(),
            Bang => (!val.as_bool().expect("Expected a bool for bang")).into(),
            _ => panic!("Unexpected unary operator"),
        }
    }

    fn visit_literal_expr(&mut self, value: &Literal) -> Literal {
        value.clone()
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Literal {
        expr.accept(self)
    }
}

impl Interpreter {
    fn eval_numeric_binop<T, F>(&self, left: Literal, right: Literal, op: F) -> Literal
    where
        F: FnOnce(f64, f64) -> T,
        T: Into<Literal>,
    {
        let l = left.as_number().expect("Left operand must be a number");
        let r = right.as_number().expect("Right operand must be a number");
        op(l, r).into()
    }

    fn eval_plus(&self, left: Literal, right: Literal) -> Literal {
        match (left, right) {
            (Literal::Number(l), Literal::Number(r)) => (l + r).into(),
            (Literal::String(l), Literal::String(r)) => (l + &r).into(),
            _ => panic!("Expected two numbers or two strings for plus operator"),
        }
    }
}
