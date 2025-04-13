use crate::ast::expr::Expr;
use crate::ast::visitor::Visitor;
use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};

use crate::runtime_error::RuntimeError;

pub struct Interpreter;

type InterpreterResult = Result<Literal, RuntimeError>;

impl Visitor<InterpreterResult> for Interpreter {
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> InterpreterResult {
        let literal = right.accept(self)?;

        use TokenType::*;
        match operator.kind {
            Minus => {
                let value = self.expect_number(literal, operator)?;
                Ok(Literal::Number(-value))
            }
            Bang => {
                let value = self.expect_bool(literal, operator)?;
                Ok(Literal::Boolean(!value))
            }
            _ => unreachable!("Unexpected unary operator"),
        }
    }

    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> InterpreterResult {
        let left_val = left.accept(self)?;
        let right_val = right.accept(self)?;

        use TokenType::*;
        match operator.kind {
            Plus => self.eval_plus(left_val, right_val, operator),
            Minus => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a - b),
            Slash => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a / b),
            Star => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a * b),
            Modulo => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a % b),

            Greater => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a > b),
            GreaterEqual => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a >= b),
            Less => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a < b),
            LessEqual => self.eval_numeric_binop(left_val, right_val, operator, |a, b| a <= b),

            BangEqual => Ok(Literal::from(!(left_val == right_val))),
            EqualEqual => Ok(Literal::from(left_val == right_val)),

            _ => unreachable!("Unknown binary operator"),
        }
    }

    fn visit_literal_expr(&mut self, value: &Literal) -> InterpreterResult {
        Ok(value.clone())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> InterpreterResult {
        expr.accept(self)
    }
}

impl Interpreter {
    pub fn interpret(&mut self, expr: &Expr) -> InterpreterResult {
        expr.accept(self)
    }

    fn eval_numeric_binop<T, F>(
        &self,
        left: Literal,
        right: Literal,
        operator: &Token,
        op: F,
    ) -> InterpreterResult
    where
        F: FnOnce(f64, f64) -> T,
        T: Into<Literal>,
    {
        let l = self.expect_number(left, operator)?;
        let r = self.expect_number(right, operator)?;
        Ok(op(l, r).into())
    }

    fn eval_plus(&self, left: Literal, right: Literal, operator: &Token) -> InterpreterResult {
        match (left, right) {
            (Literal::Number(l), Literal::Number(r)) => Ok((l + r).into()),
            (Literal::String(l), Literal::String(r)) => Ok((l + &r).into()),
            _ => Err(RuntimeError::new(
                operator,
                "Expected two numbers or two strings for '+' operator",
            )),
        }
    }

    fn expect_number(&self, literal: Literal, operator: &Token) -> Result<f64, RuntimeError> {
        literal
            .as_number()
            .ok_or(RuntimeError::new(operator, "Operand must be a number"))
    }

    fn expect_bool(&self, literal: Literal, operator: &Token) -> Result<bool, RuntimeError> {
        literal
            .as_bool()
            .ok_or(RuntimeError::new(operator, "Operand must be a boolean"))
    }
}
