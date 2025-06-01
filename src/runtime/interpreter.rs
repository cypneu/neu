use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::ControlFlow;
use std::rc::Rc;

use crate::ast::expr::Expr;
use crate::ast::stmt::{FunctionDecl, Stmt, StructDecl};
use crate::ast::{expr, stmt};
use crate::frontend::literal::Literal;
use crate::frontend::token::{Token, TokenType};
use crate::runtime::environment::Environment;
use crate::runtime::function::Function;
use crate::runtime::native::register_natives;
use crate::runtime::runtime_error::RuntimeError;
use crate::runtime::struct_declaration::StructDeclaration;
use crate::runtime::struct_instance::StructInstance;
use crate::runtime::value::Value;

use super::bound_method::BoundMethod;
use super::callable::CallableObj;

pub type EnvRef = Rc<RefCell<Environment>>;

pub struct Interpreter {
    pub environment: EnvRef,
    pub globals: EnvRef,
}

type ExprEvalResult = Result<Value, RuntimeError>;
type StmtEvalResult = Result<ControlFlow<Value>, RuntimeError>;

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
        let env = self.environment.borrow();
        let value = env.get(name)?;
        Ok(value.as_ref().clone())
    }

    fn visit_assignment_expr(&mut self, name: &Token, expr: &Expr) -> ExprEvalResult {
        let value = self.evaluate(expr)?;
        self.environment
            .borrow_mut()
            .assign(&name.lexeme, value.clone());
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

    fn visit_call_expr(&mut self, callee: &Expr, arguments: &[Expr]) -> ExprEvalResult {
        let callee = self.evaluate(callee)?;

        let mut evaluated_arguments = Vec::new();
        for argument in arguments {
            evaluated_arguments.push(self.evaluate(argument)?);
        }

        match callee {
            Value::Callable(callee) => {
                let got = evaluated_arguments.len();
                match callee.arity() {
                    Some(n) if got == n => Ok(callee.call(self, evaluated_arguments)?),
                    None => Ok(callee.call(self, evaluated_arguments)?),
                    Some(n) => {
                        let msg = format!("Expected {} arguments, but got {}.", n, got);
                        Err(RuntimeError::new(&msg))
                    }
                }
            }
            _ => Err(RuntimeError::new("Can only call functions.")),
        }
    }

    fn visit_struct_init_expr(
        &mut self,
        initializer: &Expr,
        fields: &[(Token, Expr)],
    ) -> ExprEvalResult {
        let struct_initializer = self.evaluate(initializer)?;
        if let Value::StructDecl(struct_decl) = struct_initializer {
            let mut instance = StructInstance::new(struct_decl);
            for (name, expr) in fields {
                let value = self.evaluate(expr)?;
                instance.set(name, value)?;
            }
            Ok(Value::StructInstance(Rc::new(RefCell::new(instance))))
        } else {
            Err(RuntimeError::new(
                "Expected a struct type for initialization.",
            ))
        }
    }

    fn visit_get_expr(&mut self, name: &Token, expr: &Expr) -> ExprEvalResult {
        let target = self.evaluate(expr)?;
        match target {
            Value::StructInstance(s) => {
                let value = s.borrow().get(name)?;
                if let Value::Callable(method) = value.as_ref() {
                    if let Some(0) = method.as_ref().arity() {
                        return Err(RuntimeError::with_token(
                            name,
                            "Static method cannot be called on an instance.",
                        ));
                    }
                    let bound =
                        BoundMethod::new(Value::StructInstance(s.clone()), Rc::clone(method));
                    Ok(Value::Callable(bound))
                } else {
                    Ok(value.as_ref().clone())
                }
            }
            Value::StructDecl(decl) => {
                if let Some(method) = decl.methods.get(&name.lexeme) {
                    Ok(Value::Callable(Rc::clone(method)))
                } else {
                    Err(RuntimeError::with_token(
                        name,
                        &format!("Undefined property '{}'.", name.lexeme),
                    ))
                }
            }
            _ => Err(RuntimeError::with_token(
                name,
                "Only structs or struct instances have properties.",
            )),
        }
    }

    fn visit_set_expr(&mut self, name: &Token, expr: &Expr, value: &Expr) -> ExprEvalResult {
        let struct_instance = self.evaluate(expr)?;

        if let Value::StructInstance(s) = struct_instance {
            let value = self.evaluate(value)?;
            s.borrow_mut().set(name, value.clone())?;
            Ok(value)
        } else {
            Err(RuntimeError::with_token(
                name,
                "Only instances have fields.",
            ))
        }
    }
}

impl stmt::Visitor<StmtEvalResult> for Interpreter {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> StmtEvalResult {
        self.evaluate(expr)?;
        Ok(ControlFlow::Continue(()))
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt]) -> StmtEvalResult {
        let parent = Rc::clone(&self.environment);
        self.execute_block(stmts, Environment::new(Some(parent)))
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
            (false, None) => Ok(ControlFlow::Continue(())),
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
        Ok(ControlFlow::Continue(()))
    }

    fn visit_func_declaration(&mut self, declaration: &Rc<FunctionDecl>) -> StmtEvalResult {
        let function = Function {
            declaration: Rc::clone(declaration),
            closure: Rc::clone(&self.environment),
        };
        let callable = Rc::new(function);
        let value = Value::Callable(callable);
        self.environment
            .borrow_mut()
            .assign(&declaration.name.lexeme, value);
        Ok(ControlFlow::Continue(()))
    }

    fn visit_return_stmt(&mut self, value: &Option<Expr>) -> StmtEvalResult {
        let return_value = if let Some(value) = value {
            self.evaluate(value)?
        } else {
            Value::None
        };

        Ok(ControlFlow::Break(return_value))
    }

    fn visit_struct_declaration(&mut self, declaration: &StructDecl) -> StmtEvalResult {
        let mut environment = self.environment.borrow_mut();
        environment.define(&declaration.name.lexeme, Value::None);

        let mut methods: HashMap<String, CallableObj> = HashMap::new();
        for method in &declaration.methods {
            let function = Function {
                declaration: Rc::clone(method),
                closure: Rc::clone(&self.environment),
            };
            methods.insert(method.name.lexeme.to_string(), Rc::new(function));
        }

        let struct_decl = Value::StructDecl(StructDeclaration {
            name: declaration.name.lexeme.to_string(),
            fields: declaration
                .fields
                .iter()
                .map(|t| t.lexeme.clone())
                .collect(),
            methods,
        });
        environment.assign(&declaration.name.lexeme, struct_decl);
        Ok(ControlFlow::Continue(()))
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

    pub fn execute_block(
        &mut self,
        stmts: &[Stmt],
        environment: Environment,
    ) -> Result<ControlFlow<Value>, RuntimeError> {
        let new_env = Rc::new(RefCell::new(environment));
        let previous = std::mem::replace(&mut self.environment, new_env);

        let result = (|| {
            for stmt in stmts {
                let cf = self.execute(stmt)?;
                if cf != ControlFlow::Continue(()) {
                    return Ok(cf);
                }
            }
            Ok(ControlFlow::Continue(()))
        })();

        self.environment = previous;
        result
    }

    fn new() -> Self {
        let mut environment = Environment::new(None);
        register_natives(&mut environment);

        let environment = Rc::new(RefCell::new(environment));
        let globals = environment.clone();
        Interpreter {
            environment,
            globals,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::{parser::Parser, scanner::Scanner};

    fn eval(src: &str, var: &str) -> Result<Value, RuntimeError> {
        let (toks, _) = Scanner::scan(src);
        let (stmts, _) = Parser::parse(toks);

        let mut interp = Interpreter::new();
        for stmt in stmts {
            interp.execute(&stmt)?;
        }
        let tok = Token::new(TokenType::Identifier, var.to_string(), None, 0);
        let env = interp.environment.borrow();
        Ok(env.get(&tok).unwrap().as_ref().clone())
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

    #[test]
    fn for_loop_sums() {
        let src = r#"
            sum = 0;
            for i in 0..5 {
                sum = sum + i;    // 0+1+2+3+4 = 10
            }
        "#;
        assert_eq!(eval(src, "sum").unwrap(), Value::Number(10.0));
    }

    #[test]
    fn function_call_add() {
        let src = r#"
            fn add(a, b) { return a + b; }
            result = add(2, 3);
        "#;
        assert_eq!(eval(src, "result").unwrap(), Value::Number(5.0));
    }

    #[test]
    fn recursive_function_fib() {
        let src = r#"
            fn fib(n) {
                if n < 2 { return n; }
                return fib(n-1) + fib(n-2);
            }
            eight = fib(6);    // 8
        "#;
        assert_eq!(eval(src, "eight").unwrap(), Value::Number(8.0));
    }

    #[test]
    fn arity_mismatch_produces_error() {
        let err = eval(r#"fn id(x) { return x; } bad = id(1, 2);"#, "bad").unwrap_err();
        assert!(
            err.message.contains("Expected 1 arguments"),
            "unexpected error: {:?}",
            err
        );
    }

    #[test]
    fn function_parameter_assignment_is_local_and_shadows_global() {
        let explicit_shadow_src = r#"
            global_val = 100;

            fn my_function(shadow_param) {
                shadow_param = 200;
                global_val = global_val + shadow_param;
            }

            outer_shadow_var = 50;
            fn another_function(outer_shadow_var) {
                outer_shadow_var = 60;
            }

            my_function(global_val);
            another_function(outer_shadow_var);
        "#;
        assert_eq!(
            eval(explicit_shadow_src, "global_val").unwrap(),
            Value::Number(300.0),
            "Global variable modified correctly from within a function that has shadowed parameters."
        );
        assert_eq!(
            eval(explicit_shadow_src, "outer_shadow_var").unwrap(),
            Value::Number(50.0),
            "Outer variable should not be affected by assignment to a shadowed parameter in a different function."
        );
    }
}
