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

    fn eval(src: &str) -> Value {
        let mut neu = Neu::new();
        let toks = Scanner::scan(src, &mut neu);
        let mut stmts = Parser::parse(toks, &mut neu);

        let mut interp = Interpreter::new();
        let Stmt::Expr(expr) = stmts.remove(0) else {
            unreachable!()
        };
        interp.evaluate(&expr).unwrap()
    }

    #[test]
    fn arithmetic_add() {
        assert_eq!(eval("1 + 2;"), Value::Number(3.0));
    }

    #[test]
    fn string_concat() {
        assert_eq!(eval("\"ab\" + \"cd\";"), Value::String("abcd".into()));
    }

    #[test]
    fn modulo_precedence() {
        assert_eq!(eval("5 * (3 + 4) % 3;"), Value::Number(2.0));
    }

    #[test]
    fn boolean_not() {
        assert_eq!(eval("!false;"), Value::Boolean(true));
    }

    #[test]
    fn variable_scope() {
        let src = "
            x = 1;
            {
               x = 2;
               y = x;
            }
            z = x;
        ";

        let mut neu = Neu::new();
        let tokens = Scanner::scan(src, &mut neu);
        let stmts = Parser::parse(tokens, &mut neu);

        let mut interp = Interpreter::new();
        for stmt in stmts {
            interp.execute(&stmt).unwrap();
        }

        let token = Token::new("x".into(), None, 0);
        let gv = interp.environment.get(&token).unwrap();
        assert_eq!(gv, &Value::Number(2.0));
    }
}
