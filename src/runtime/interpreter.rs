use std::cell::RefCell;
use std::collections::HashMap;
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

pub enum ExecutionFlow {
    Normal,
    Break,
    Continue,
    Return(Value),
}

type ExprEvalResult = Result<Value, RuntimeError>;
type StmtEvalResult = Result<ExecutionFlow, RuntimeError>;

impl Interpreter {
    pub fn interpret(statements: Vec<Stmt>) -> Result<(), Vec<RuntimeError>> {
        let mut interpreter = Interpreter::new();

        let mut errors = Vec::new();
        for statement in statements {
            if let Err(err) = interpreter.execute(&statement) {
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn execute_callable_body(
        &mut self,
        stmts: &[Stmt],
        environment: Environment,
    ) -> StmtEvalResult {
        let new_env = Rc::new(RefCell::new(environment));
        self.with_new_environment(new_env, |interpreter| interpreter.execute_statements(stmts))
    }
}

impl stmt::Visitor<StmtEvalResult> for Interpreter {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> StmtEvalResult {
        self.evaluate(expr)?;
        Ok(ExecutionFlow::Normal)
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt]) -> StmtEvalResult {
        self.execute_statements(stmts)
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
            (false, None) => Ok(ExecutionFlow::Normal),
        }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> StmtEvalResult {
        let parent = Rc::clone(&self.environment);
        let new_env = Rc::new(RefCell::new(Environment::new(Some(parent))));

        self.with_new_environment(new_env, |interpreter| {
            while interpreter
                .evaluate(condition)?
                .as_bool()
                .ok_or_else(|| RuntimeError::new("Loop condition must evaluate to a boolean"))?
            {
                match interpreter.execute(body)? {
                    ExecutionFlow::Normal | ExecutionFlow::Continue => continue,
                    ExecutionFlow::Break => break,
                    ret @ ExecutionFlow::Return(_) => return Ok(ret),
                }
            }

            Ok(ExecutionFlow::Normal)
        })
    }

    fn visit_for_stmt(
        &mut self,
        var: &Token,
        start: &Expr,
        end: &Expr,
        body: &Stmt,
    ) -> StmtEvalResult {
        let parent = Rc::clone(&self.environment);
        let new_env = Rc::new(RefCell::new(Environment::new(Some(parent))));

        self.with_new_environment(new_env, |interpreter| {
            // FIXME: handle errors, must be Number type
            let mut i = interpreter.evaluate(start)?.as_number().unwrap();
            let end = interpreter.evaluate(end)?.as_number().unwrap();

            interpreter
                .environment
                .borrow_mut()
                .assign(&var.lexeme, Value::Number(i));

            while i < end {
                match interpreter.execute(body)? {
                    ExecutionFlow::Break => break,
                    ret @ ExecutionFlow::Return(_) => return Ok(ret),
                    ExecutionFlow::Normal | ExecutionFlow::Continue => {}
                }
                i += 1.0;
                interpreter
                    .environment
                    .borrow_mut()
                    .assign(&var.lexeme, Value::Number(i));
            }

            Ok(ExecutionFlow::Normal)
        })
    }

    fn visit_break_stmt(&mut self) -> StmtEvalResult {
        Ok(ExecutionFlow::Break)
    }

    fn visit_continue_stmt(&mut self) -> StmtEvalResult {
        Ok(ExecutionFlow::Continue)
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
        Ok(ExecutionFlow::Normal)
    }

    fn visit_return_stmt(&mut self, value: &Option<Expr>) -> StmtEvalResult {
        let return_value = if let Some(value) = value {
            self.evaluate(value)?
        } else {
            Value::None
        };
        Ok(ExecutionFlow::Return(return_value))
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
        Ok(ExecutionFlow::Normal)
    }
}

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
        Ok(value)
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
        let Value::StructDecl(struct_decl) = struct_initializer else {
            let msg = "Expected a struct type for initialization.";
            return Err(RuntimeError::new(msg));
        };

        if struct_decl.fields.len() != fields.len() {
            let msg = "All struct variables must be initialized exactly once.";
            return Err(RuntimeError::new(msg));
        }

        let mut instance = StructInstance::new(struct_decl);

        for (name, expr) in fields {
            let value = self.evaluate(expr)?;
            instance.set(name, value)?;
        }

        Ok(Value::StructInstance(Rc::new(RefCell::new(instance))))
    }

    fn visit_get_expr(&mut self, name: &Token, expr: &Expr) -> ExprEvalResult {
        let target = self.evaluate(expr)?;
        match target {
            Value::StructInstance(instance) => self.property_from_instance(name, instance),
            Value::StructDecl(decl) => self.property_from_struct_decl(name, &decl),
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

impl Interpreter {
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

    fn with_new_environment<F, R>(&mut self, environment: EnvRef, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let previous = std::mem::replace(&mut self.environment, environment);

        let result = f(self);

        self.environment = previous;
        result
    }

    fn property_from_instance(
        &self,
        name: &Token,
        instance: Rc<RefCell<StructInstance>>,
    ) -> ExprEvalResult {
        let value = instance.borrow().get(name)?;
        match value.as_ref() {
            Value::Callable(method) => self.bind_instance_method(name, method, instance),
            _ => Ok(value.as_ref().clone()),
        }
    }

    fn property_from_struct_decl(&self, name: &Token, decl: &StructDeclaration) -> ExprEvalResult {
        decl.methods
            .get(&name.lexeme)
            .map(|m| Value::Callable(Rc::clone(m)))
            .ok_or_else(|| {
                RuntimeError::with_token(name, &format!("Undefined property '{}'.", name.lexeme))
            })
    }

    fn bind_instance_method(
        &self,
        name: &Token,
        method: &CallableObj,
        receiver: Rc<RefCell<StructInstance>>,
    ) -> ExprEvalResult {
        if method.arity() == Some(0) {
            Err(RuntimeError::with_token(
                name,
                "Static method cannot be called on an instance.",
            ))
        } else {
            Ok(Value::Callable(BoundMethod::new(
                Value::StructInstance(receiver),
                Rc::clone(method),
            )))
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

    fn execute_statements(&mut self, stmts: &[Stmt]) -> StmtEvalResult {
        let mut final_flow = ExecutionFlow::Normal;
        for stmt in stmts {
            match self.execute(stmt)? {
                ExecutionFlow::Normal => continue,
                flow => {
                    final_flow = flow;
                    break;
                }
            }
        }

        Ok(final_flow)
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
        Ok(env.get(&tok).unwrap())
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

    #[test]
    fn test_struct_declaration_and_instantiation_empty() {
        let src = "struct T {} x = T {};";
        let val = eval(src, "x").unwrap();
        match val {
            Value::StructInstance(inst_rc) => {
                assert_eq!(inst_rc.borrow().struct_decl.name, "T");
            }
            _ => panic!("Expected StructInstance, got {:?}", val),
        }
    }

    #[test]
    fn test_struct_instantiation_with_fields_get_set() {
        let src = r#"
            struct Point { x, y }
            p = Point { x:1, y:2 };
            val_x = p.x;
            p.y = 3;
            p.x = 5;
            val_y = p.y;
        "#;
        assert_eq!(eval(src, "val_x").unwrap(), Value::Number(1.0));
        assert_eq!(eval(src, "val_y").unwrap(), Value::Number(3.0));
    }

    #[test]
    fn test_struct_method_call_receiver_as_first_arg() {
        let src = r#"
            struct Counter {
                val,
                fn increment(self) { self.val = self.val + 1; }
                fn get(self) { return self.val; }
            }
            instance = Counter { val: 10 };
            instance.increment(); // val becomes 11
            instance.increment(); // val becomes 12
            result = instance.get();
        "#;
        assert_eq!(eval(src, "result").unwrap(), Value::Number(12.0));
    }

    #[test]
    fn test_struct_static_method_call() {
        let src = r#"
            struct MathUtils {
                fn add(a, b) { return a + b; }
            }
            sum = MathUtils.add(5, 7);
        "#;
        assert_eq!(eval(src, "sum").unwrap(), Value::Number(12.0));
    }

    #[test]
    fn error_access_non_existent_field() {
        let src = "struct S {f} i = S{f:1}; x = i.non_field;";
        let err = eval(src, "x").unwrap_err();
        assert!(err.message.contains("Undefined property 'non_field'"));
    }

    #[test]
    fn error_set_non_existent_field() {
        let src = "struct S {f} i = S{f:1}; i.non_field = 2;";
        let err = eval(src, "x").unwrap_err();
        assert!(err
            .message
            .contains("'non_field' is not a field of struct S"));
    }

    #[test]
    fn error_init_non_existent_field() {
        let src = "struct S {} i = S { non_field: 1 };";
        let err = eval(src, "i").unwrap_err();
        assert!(err
            .message
            .contains("All struct variables must be initialized exactly once."));
    }

    #[test]
    fn error_type_mismatch_add_num_str() {
        let err = eval("x = 1 + \"s\";", "x").unwrap_err();
        assert!(err
            .message
            .contains("Expected two numbers or two strings for '+' operator"));
    }

    #[test]
    fn error_unary_negate_string() {
        let err = eval("x = -\"s\";", "x").unwrap_err();
        assert!(err.message.contains("Operand must be a number"));
    }

    #[test]
    fn error_logical_operand_not_boolean() {
        let err = eval("x = 1 and true;", "x").unwrap_err();
        assert!(err.message.contains("Logical operands must be a boolean."));
    }

    #[test]
    fn error_if_condition_not_boolean() {
        let err = eval("if 1 { x=1; } else {x=2;}", "x").unwrap_err();
        assert!(err.message.contains("If condition must be a boolean."));
    }

    #[test]
    fn error_call_non_callable() {
        let err = eval("y=10; x = y();", "x").unwrap_err();
        assert!(err.message.contains("Can only call functions."));
    }

    #[test]
    fn error_init_missing_field() {
        let src = "struct Point {x,y} p = Point {x:1};";
        let err = eval(src, "p").unwrap_err();
        assert!(err
            .message
            .contains("All struct variables must be initialized exactly once."));
    }

    #[test]
    fn error_init_duplicate_field_in_literal() {
        let src = "struct Point {x,y} p = Point {x:1, y:2, x:3};";
        let err = eval(src, "p").unwrap_err();
        assert!(err
            .message
            .contains("All struct variables must be initialized exactly once."));
    }
}
