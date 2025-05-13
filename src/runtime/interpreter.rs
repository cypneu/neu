use crate::ast::expr::Expr;
use crate::ast::stmt::Stmt;
use crate::ast::{expr, stmt};
use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};
use crate::runtime::environment::Environment;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::value::Value;

pub struct Interpreter {
    pub environment: Box<Environment>,
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
        self.environment.assign(name, value.clone());
        Ok(value)
    }

    fn visit_logical_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> ExprEvalResult {
        let left_val = self.evaluate(left)?;
        let left_bool = left_val
            .as_bool()
            .ok_or_else(|| RuntimeError::new("Logical operands must be a boolean."))?;

        let eval_right = if op.kind == TokenType::Or {
            !left_bool
        } else {
            left_bool
        };

        if eval_right {
            self.evaluate(right)
        } else {
            Ok(left_val)
        }
    }
}

impl stmt::Visitor<StmtEvalResult> for Interpreter {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> StmtEvalResult {
        let value = self.evaluate(expr)?;
        println!("Expr stmt: {:?}\nValue: {:?}\n", expr, value);
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt]) -> StmtEvalResult {
        self.push_env();
        let result = stmts.iter().try_for_each(|stmt| self.execute(stmt));
        self.pop_env();
        result
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> StmtEvalResult {
        let truth = self
            .evaluate(condition)?
            .as_bool()
            .ok_or_else(|| RuntimeError::new("If condition must be a boolean."))?;

        match (truth, else_branch) {
            (true, _) => self.execute(then_branch),
            (false, Some(stmt)) => self.execute(stmt),
            (false, None) => Ok(()),
        }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> StmtEvalResult {
        while self
            .evaluate(condition)?
            .as_bool()
            .ok_or_else(|| RuntimeError::new("Loop condition must evaluate to a boolean"))?
        {
            self.execute(body)?;
        }
        Ok(())
    }
}

impl Interpreter {
    pub fn interpret(statements: Vec<Stmt>) {
        let mut interpreter = Interpreter::new();
        for statement in statements {
            if let Err(err) = interpreter.execute(&statement) {
                println!("Err: {:?}", err);
            }
        }
    }

    fn new() -> Self {
        let environment = Box::new(Environment::new(None));
        Interpreter { environment }
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
            _ => Err(RuntimeError::with_token(
                operator,
                "Expected two numbers or two strings for '+' operator",
            )),
        }
    }

    fn expect_number(&self, value: Value, operator: &Token) -> Result<f64, RuntimeError> {
        value.as_number().ok_or(RuntimeError::with_token(
            operator,
            "Operand must be a number",
        ))
    }

    fn expect_bool(&self, value: Value, operator: &Token) -> Result<bool, RuntimeError> {
        value.as_bool().ok_or(RuntimeError::with_token(
            operator,
            "Operand must be a boolean",
        ))
    }

    fn evaluate(&mut self, expr: &Expr) -> ExprEvalResult {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: &Stmt) -> StmtEvalResult {
        stmt.accept(self)
    }

    fn push_env(&mut self) {
        let parent = std::mem::replace(&mut self.environment, Box::new(Environment::new(None)));
        self.environment = Box::new(Environment::new(Some(parent)));
    }

    fn pop_env(&mut self) {
        let current = std::mem::replace(&mut self.environment, Box::new(Environment::new(None)));
        self.environment = current.enclosing.expect("no enclosing environment");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::{parser::Parser, scanner::Scanner};
    use crate::neu::Neu;

    fn eval(src: &str, var: &str) -> Result<Value, RuntimeError> {
        let mut neu = Neu::new();
        let toks = Scanner::scan(src, &mut neu);
        let stmts = Parser::parse(toks, &mut neu);

        let mut interp = Interpreter::new();
        for stmt in stmts {
            interp.execute(&stmt)?;
        }
        let tok = Token::new(var.to_string(), None, 0);
        Ok(interp.environment.get(&tok).unwrap().clone())
    }

    #[test]
    fn basic_expression_evaluation() {
        let cases: &[(&str, Value)] = &[
            ("1 + 2", Value::Number(3.0)),
            ("\"ab\" + \"cd\"", Value::String("abcd".into())),
            ("5 * (3 + 4) % 3", Value::Number(2.0)),
            ("!false", Value::Boolean(true)),
        ];

        for (expr, expected) in cases {
            let src = format!("x = {};;", expr);
            let got = eval(&src, "x").unwrap();
            assert_eq!(got, *expected, "evaluating `{}`", expr);
        }
    }

    #[test]
    fn logical_and_evaluation() {
        assert_eq!(
            eval("x = true and true;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            eval("x = true and false;", "x").unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            eval("x = false and true;", "x").unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            eval("x = false and false;", "x").unwrap(),
            Value::Boolean(false)
        );
    }

    #[test]
    fn logical_or_evaluation() {
        assert_eq!(
            eval("x = true or true;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            eval("x = true or false;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            eval("x = false or true;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            eval("x = false or false;", "x").unwrap(),
            Value::Boolean(false)
        );
    }

    #[test]
    fn logical_precedence() {
        assert_eq!(
            eval("x = false or true and false;", "x").unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            eval("x = true and false or true;", "x").unwrap(),
            Value::Boolean(true)
        );
    }

    #[test]
    fn logical_short_circuit_does_not_evaluate_rhs() {
        let a = eval("x = false and undefinedVar;", "x").unwrap();
        assert_eq!(a, Value::Boolean(false));

        let b = eval("x = true or undefinedVar;", "x").unwrap();
        assert_eq!(b, Value::Boolean(true));
    }

    #[test]
    fn variable_scope() {
        let src = r#"
            x = 1;
            {
                x = 2;
                y = x;
            }
            z = x;
        "#;
        assert_eq!(eval(src, "x").unwrap(), Value::Number(2.0));
    }

    #[test]
    fn if_true_executes_then_branch() {
        assert_eq!(
            eval("x = 0; if true { x = 1; } else { x = 2; }", "x").unwrap(),
            Value::Number(1.0)
        );
    }

    #[test]
    fn else_if_executes_first_true_branch() {
        let src = r#"
            x = 0;
            if false  { x = 1; }
            else if false {}
            else if true  { x = 2; }
            else          { x = 3; }
        "#;
        assert_eq!(eval(src, "x").unwrap(), Value::Number(2.0));
    }

    #[test]
    fn else_branch_executes_when_condition_false() {
        assert_eq!(
            eval("x = 0; if false { x = 1; } else { x = 2; }", "x").unwrap(),
            Value::Number(2.0)
        );
    }

    #[test]
    fn while_loop_runs_until_false() {
        let src = r#"
        counter = 0;
        while counter < 3 {
            counter = counter + 1;
        }
    "#;
        assert_eq!(eval(src, "counter").unwrap(), Value::Number(3.0));
    }

    #[test]
    fn while_condition_false_initially() {
        let src = r#"
        i = 0;
        while i > 0 { i = i - 1; }
    "#;
        assert_eq!(eval(src, "i").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn while_loop_non_boolean_condition() {
        let err = eval("while 1 { }", "x").unwrap_err();
        assert!(
            err.message
                .contains("Loop condition must evaluate to a boolean"),
            "unexpected error message: {:?}",
            err
        );
    }
}
