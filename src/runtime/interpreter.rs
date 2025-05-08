use crate::ast::expr::Expr;
use crate::ast::stmt::Stmt;
use crate::ast::{expr, stmt};
use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};
use crate::runtime::environment::Environment;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

pub struct Interpreter {
    environment: Environment,
}

type ExprEvalResult = Result<Value, RuntimeError>;
type StmtEvalResult = Result<(), RuntimeError>;

impl expr::Visitor<ExprEvalResult> for Interpreter {
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> ExprEvalResult {
        let value = self.evaluate(right)?;

        use TokenType::*;
        match operator.kind {
            Minus => {
                let value = self.expect_number(value, operator)?;
                Ok(Value::Number(-value))
            }
            Bang => {
                let value = self.expect_bool(value, operator)?;
                Ok(Value::Boolean(!value))
            }
            _ => unreachable!("Unexpected unary operator"),
        }
    }

    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> ExprEvalResult {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;

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

            BangEqual => Ok(Value::Boolean(!(left_val == right_val))),
            EqualEqual => Ok(Value::Boolean(left_val == right_val)),

            _ => unreachable!("Unknown binary operator"),
        }
    }

    fn visit_literal_expr(&mut self, value: &Literal) -> ExprEvalResult {
        Ok(Value::from(value.clone()))
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> ExprEvalResult {
        self.evaluate(expr)
    }

    fn visit_variable_expr(&mut self, name: &Token) -> ExprEvalResult {
        let value = self.environment.get(name)?;
        Ok(value.clone())
    }

    fn visit_assignment_expr(&mut self, name: &Token, expr: &Expr) -> ExprEvalResult {
        let value = self.evaluate(expr)?;
        self.environment.assign(name.lexeme.clone(), value.clone());
        Ok(value)
    }
}

impl stmt::Visitor<StmtEvalResult> for Interpreter {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> StmtEvalResult {
        println!("Expr stmt: {expr:#?}");
        let value = self.evaluate(expr)?;
        println!("Expr stmt value: {:?}\n", value);
        Ok(())
    }
}

impl Interpreter {
    pub fn interpret(statements: Vec<Stmt>) {
        let mut interpreter = Interpreter::new();
        for statement in statements {
            if let Err(err) = interpreter.execute(statement) {
                println!("Err: {:?}", err);
            }
        }
    }
    fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
        }
    }

    fn eval_numeric_binop<T, F>(
        &self,
        left: Value,
        right: Value,
        operator: &Token,
        op: F,
    ) -> ExprEvalResult
    where
        F: FnOnce(f64, f64) -> T,
        T: Into<Value>,
    {
        let l = self.expect_number(left, operator)?;
        let r = self.expect_number(right, operator)?;
        Ok(op(l, r).into())
    }

    fn eval_plus(&self, left: Value, right: Value, operator: &Token) -> ExprEvalResult {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
            (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
            _ => Err(RuntimeError::new(
                operator,
                "Expected two numbers or two strings for '+' operator",
            )),
        }
    }

    fn expect_number(&self, value: Value, operator: &Token) -> Result<f64, RuntimeError> {
        value
            .as_number()
            .ok_or(RuntimeError::new(operator, "Operand must be a number"))
    }

    fn expect_bool(&self, value: Value, operator: &Token) -> Result<bool, RuntimeError> {
        value
            .as_bool()
            .ok_or(RuntimeError::new(operator, "Operand must be a boolean"))
    }

    fn evaluate(&mut self, expr: &Expr) -> ExprEvalResult {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: Stmt) -> StmtEvalResult {
        stmt.accept(self)
    }
}
