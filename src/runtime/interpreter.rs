use std::cell::RefCell;
use std::cmp::Ordering;
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

pub type EnvRef<'src> = Rc<RefCell<Environment<'src>>>;

pub struct Interpreter<'src> {
    pub environment: EnvRef<'src>,
    pub globals: EnvRef<'src>,
}

pub enum ExecutionFlow<'src> {
    Normal,
    Break,
    Continue,
    Return(Rc<Value<'src>>),
}

type ExprEvalResult<'src> = Result<Rc<Value<'src>>, RuntimeError<'src>>;
type StmtEvalResult<'src> = Result<ExecutionFlow<'src>, RuntimeError<'src>>;

impl<'src> Interpreter<'src> {
    pub fn interpret(statements: Vec<Stmt<'src>>) -> Result<(), Vec<RuntimeError<'src>>> {
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
        stmts: &[Stmt<'src>],
        environment: Environment<'src>,
    ) -> StmtEvalResult<'src> {
        let new_env = Rc::new(RefCell::new(environment));
        self.with_new_environment(new_env, |interpreter| interpreter.execute_statements(stmts))
    }
}

impl<'src> stmt::Visitor<'src, StmtEvalResult<'src>> for Interpreter<'src> {
    fn visit_expression_stmt(&mut self, expr: &Expr<'src>) -> StmtEvalResult<'src> {
        self.evaluate(expr)?;
        Ok(ExecutionFlow::Normal)
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt<'src>]) -> StmtEvalResult<'src> {
        self.execute_statements(stmts)
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr<'src>,
        then_branch: &Stmt<'src>,
        else_branch: Option<&Stmt<'src>>,
    ) -> StmtEvalResult<'src> {
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

    fn visit_while_stmt(
        &mut self,
        condition: &Expr<'src>,
        body: &Stmt<'src>,
    ) -> StmtEvalResult<'src> {
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
        var: &Token<'src>,
        start: &Expr<'src>,
        end: &Expr<'src>,
        body: &Stmt<'src>,
    ) -> StmtEvalResult<'src> {
        let start_val = self.evaluate(start)?;
        let mut i = self.expect_integer(start_val, var)?;

        let end_val = self.evaluate(end)?;
        let end = self.expect_integer(end_val, var)?;

        let parent = Rc::clone(&self.environment);
        let new_env = Rc::new(RefCell::new(Environment::new(Some(parent))));

        self.with_new_environment(new_env, |interpreter| {
            interpreter
                .environment
                .borrow_mut()
                .assign(var.lexeme, Rc::new(Value::Number(i as f64)));

            while i < end {
                match interpreter.execute(body)? {
                    ExecutionFlow::Break => break,
                    ret @ ExecutionFlow::Return(_) => return Ok(ret),
                    ExecutionFlow::Normal | ExecutionFlow::Continue => {}
                }
                i += 1;
                interpreter
                    .environment
                    .borrow_mut()
                    .assign(var.lexeme, Rc::new(Value::Number(i as f64)));
            }

            Ok(ExecutionFlow::Normal)
        })
    }

    fn visit_break_stmt(&mut self) -> StmtEvalResult<'src> {
        Ok(ExecutionFlow::Break)
    }

    fn visit_continue_stmt(&mut self) -> StmtEvalResult<'src> {
        Ok(ExecutionFlow::Continue)
    }

    fn visit_func_declaration(
        &mut self,
        declaration: &Rc<FunctionDecl<'src>>,
    ) -> StmtEvalResult<'src> {
        let function = Function {
            declaration: Rc::clone(declaration),
            closure: Rc::clone(&self.environment),
        };
        let callable = Rc::new(function);
        let value = Value::Callable(callable);
        self.environment
            .borrow_mut()
            .assign(declaration.name.lexeme, Rc::new(value));
        Ok(ExecutionFlow::Normal)
    }

    fn visit_return_stmt(&mut self, value: &Option<Expr<'src>>) -> StmtEvalResult<'src> {
        let return_value = if let Some(value) = value {
            self.evaluate(value)?
        } else {
            Rc::new(Value::None)
        };
        Ok(ExecutionFlow::Return(return_value))
    }

    fn visit_struct_declaration(&mut self, declaration: &StructDecl<'src>) -> StmtEvalResult<'src> {
        let mut environment = self.environment.borrow_mut();
        environment.define(declaration.name.lexeme, Rc::new(Value::None));

        let mut methods: HashMap<&str, CallableObj> = HashMap::new();
        for method in &declaration.methods {
            let function = Function {
                declaration: Rc::clone(method),
                closure: Rc::clone(&self.environment),
            };
            methods.insert(method.name.lexeme, Rc::new(function));
        }

        let struct_decl = Value::StructDecl(StructDeclaration {
            name: declaration.name.lexeme,
            fields: declaration.fields.iter().map(|t| t.lexeme).collect(),
            methods,
        });
        environment.assign(declaration.name.lexeme, Rc::new(struct_decl));
        Ok(ExecutionFlow::Normal)
    }
}

impl<'src> expr::Visitor<'src, ExprEvalResult<'src>> for Interpreter<'src> {
    fn visit_unary_expr(
        &mut self,
        operator: &Token<'src>,
        right: &Expr<'src>,
    ) -> ExprEvalResult<'src> {
        let value = self.evaluate(right)?;

        use TokenType::*;
        match operator.kind {
            Minus => {
                let value = self.expect_number(&value, operator)?;
                Ok(Rc::new(Value::Number(-value)))
            }
            Bang => {
                let value = self.expect_bool(value, operator)?;
                Ok(Rc::new(Value::Boolean(!value)))
            }
            _ => unreachable!("Unexpected unary operator"),
        }
    }

    fn visit_binary_expr(
        &mut self,
        left: &Expr<'src>,
        operator: &Token<'src>,
        right: &Expr<'src>,
    ) -> ExprEvalResult<'src> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        use TokenType::*;
        match operator.kind {
            Plus => self.eval_plus(left, right, operator),
            Minus => self.eval_numeric_binop(left, right, operator, |a, b| a - b),
            Slash => self.eval_numeric_binop(left, right, operator, |a, b| a / b),
            Star => self.eval_numeric_binop(left, right, operator, |a, b| a * b),
            Modulo => self.eval_numeric_binop(left, right, operator, |a, b| a % b),

            Greater => self.eval_order_binop(left, right, operator, |o| o == Ordering::Greater),
            GreaterEqual => self.eval_order_binop(left, right, operator, |o| {
                matches!(o, Ordering::Greater | Ordering::Equal)
            }),
            Less => self.eval_order_binop(left, right, operator, |o| o == Ordering::Less),
            LessEqual => self.eval_order_binop(left, right, operator, |o| {
                matches!(o, Ordering::Less | Ordering::Equal)
            }),

            BangEqual => Ok(Rc::new(Value::Boolean(!(left == right)))),
            EqualEqual => Ok(Rc::new(Value::Boolean(left == right))),

            _ => unreachable!("Unknown binary operator"),
        }
    }

    fn visit_literal_expr(&mut self, value: &Literal<'src>) -> ExprEvalResult<'src> {
        Ok(Rc::new(Value::from(value.clone())))
    }

    fn visit_grouping_expr(&mut self, expr: &Expr<'src>) -> ExprEvalResult<'src> {
        self.evaluate(expr)
    }

    fn visit_variable_expr(&mut self, name: &Token<'src>) -> ExprEvalResult<'src> {
        let env = self.environment.borrow();
        env.get(name)
    }

    fn visit_assignment_expr(
        &mut self,
        name: &Token<'src>,
        expr: &Expr<'src>,
    ) -> ExprEvalResult<'src> {
        let value = self.evaluate(expr)?;
        self.environment
            .borrow_mut()
            .assign(name.lexeme, Rc::clone(&value));
        Ok(value)
    }

    fn visit_logical_expr(
        &mut self,
        left: &Expr<'src>,
        op: &Token<'src>,
        right: &Expr<'src>,
    ) -> ExprEvalResult<'src> {
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

    fn visit_call_expr(
        &mut self,
        callee: &Expr<'src>,
        arguments: &[Expr<'src>],
    ) -> ExprEvalResult<'src> {
        let callee = self.evaluate(callee)?;

        let callable = match &*callee {
            Value::Callable(f) => f,
            other => {
                let msg = format!("Attempted to call a non-callable value: {:?}", other);
                return Err(RuntimeError::new(&msg));
            }
        };

        let mut evaluated_arguments = Vec::with_capacity(arguments.len());
        for argument in arguments {
            evaluated_arguments.push(self.evaluate(argument)?);
        }

        if let Some(expected) = callable.arity() {
            let got = evaluated_arguments.len();
            if got != expected {
                let msg = format!("Expected {} arguments, but got {}.", expected, got);
                return Err(RuntimeError::new(&msg));
            }
        }

        callable.call(self, evaluated_arguments)
    }

    fn visit_struct_init_expr(
        &mut self,
        initializer: &Expr<'src>,
        fields: &[(Token<'src>, Expr<'src>)],
    ) -> ExprEvalResult<'src> {
        let struct_initializer = self.evaluate(initializer)?;
        let struct_decl = match &*struct_initializer {
            Value::StructDecl(decl) => decl.clone(),
            _ => {
                let msg = "Expected a struct type for initialization.";
                return Err(RuntimeError::new(msg));
            }
        };

        if struct_decl.fields.len() != fields.len() {
            let msg = "All and only defined struct variables must be initialized.";
            return Err(RuntimeError::new(msg));
        }

        let mut instance = StructInstance::new(struct_decl);
        for (name, expr) in fields {
            let value = self.evaluate(expr)?;
            instance.set(name, value)?;
        }

        let struct_instance = Value::StructInstance(Rc::new(RefCell::new(instance)));
        Ok(Rc::new(struct_instance))
    }

    fn visit_get_expr(&mut self, name: &Token<'src>, expr: &Expr<'src>) -> ExprEvalResult<'src> {
        let target = self.evaluate(expr)?;
        match &*target {
            Value::StructInstance(instance) => self.property_from_instance(name, instance),
            Value::StructDecl(decl) => self.property_from_struct_decl(name, decl),
            _ => Err(RuntimeError::with_token(
                name,
                "Only structs or struct instances have properties.",
            )),
        }
    }

    fn visit_set_expr(
        &mut self,
        name: &Token<'src>,
        expr: &Expr<'src>,
        value: &Expr<'src>,
    ) -> ExprEvalResult<'src> {
        let struct_instance = self.evaluate(expr)?;

        if let Value::StructInstance(s) = &*struct_instance {
            let value = self.evaluate(value)?;
            s.borrow_mut().set(name, value.clone())?;
            Ok(value)
        } else {
            let msg = "Only instances have fields.";
            Err(RuntimeError::with_token(name, msg))
        }
    }
}

impl<'src> Interpreter<'src> {
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

    fn with_new_environment<F, R>(&mut self, environment: EnvRef<'src>, f: F) -> R
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
        name: &Token<'src>,
        instance: &Rc<RefCell<StructInstance<'src>>>,
    ) -> ExprEvalResult<'src> {
        let value = instance.borrow().get(name)?;

        if instance
            .borrow()
            .struct_decl
            .methods
            .contains_key(name.lexeme)
        {
            if let Value::Callable(method) = value.as_ref() {
                return self.bind_instance_method(name, method, instance);
            }
        }

        Ok(value)
    }

    fn property_from_struct_decl(
        &self,
        name: &Token<'src>,
        decl: &StructDeclaration<'src>,
    ) -> ExprEvalResult<'src> {
        decl.methods
            .get(name.lexeme)
            .map(|m| Rc::new(Value::Callable(Rc::clone(m))))
            .ok_or_else(|| {
                RuntimeError::with_token(name, &format!("Undefined property '{}'.", name.lexeme))
            })
    }

    fn bind_instance_method(
        &self,
        name: &Token<'src>,
        method: &CallableObj<'src>,
        receiver: &Rc<RefCell<StructInstance<'src>>>,
    ) -> ExprEvalResult<'src> {
        if method.arity() == Some(0) {
            let msg = "Static method cannot be called on an instance.";
            Err(RuntimeError::with_token(name, msg))
        } else {
            let bm = BoundMethod::new(
                Rc::new(Value::StructInstance(Rc::clone(receiver))),
                Rc::clone(method),
            );
            Ok(Rc::new(Value::Callable(bm)))
        }
    }

    fn eval_numeric_binop<T, F>(
        &self,
        left: Rc<Value<'src>>,
        right: Rc<Value<'src>>,
        operator: &Token<'src>,
        op: F,
    ) -> ExprEvalResult<'src>
    where
        F: FnOnce(f64, f64) -> T,
        T: Into<Value<'src>>,
    {
        let l = self.expect_number(&left, operator)?;
        let r = self.expect_number(&right, operator)?;
        Ok(Rc::new(op(l, r).into()))
    }

    fn eval_order_binop<F>(
        &self,
        left: Rc<Value>,
        right: Rc<Value>,
        operator: &Token<'src>,
        pred: F,
    ) -> ExprEvalResult<'src>
    where
        F: FnOnce(Ordering) -> bool,
    {
        use Value::*;
        let ord = match (&*left, &*right) {
            (Number(a), Number(b)) => a
                .partial_cmp(b)
                .ok_or_else(|| RuntimeError::with_token(operator, "NaN is unordered"))?,
            (String(a), String(b)) => a.cmp(b),
            _ => {
                return Err(RuntimeError::with_token(
                    operator,
                    "Operands must be two numbers or two strings for comparison.",
                ))
            }
        };
        Ok(Rc::new(Value::Boolean(pred(ord))))
    }

    fn eval_plus(
        &self,
        left: Rc<Value>,
        right: Rc<Value>,
        operator: &Token<'src>,
    ) -> ExprEvalResult<'src> {
        match (&*left, &*right) {
            (Value::Number(l), Value::Number(r)) => Ok(Rc::new(Value::Number(l + r))),
            (Value::String(l), Value::String(r)) => Ok(Rc::new(Value::String(l.to_owned() + r))),
            _ => Err(RuntimeError::with_token(
                operator,
                "Expected two numbers or two strings for '+' operator",
            )),
        }
    }

    fn expect_number(
        &self,
        value: &Rc<Value>,
        op: &Token<'src>,
    ) -> Result<f64, RuntimeError<'src>> {
        match &**value {
            Value::Number(n) => Ok(*n),
            _ => Err(RuntimeError::with_token(op, "Operand must be a number")),
        }
    }

    fn expect_integer(
        &self,
        value: Rc<Value>,
        token: &Token<'src>,
    ) -> Result<i64, RuntimeError<'src>> {
        value
            .as_integer()
            .ok_or_else(|| RuntimeError::with_token(token, "Expected integer."))
    }

    fn expect_bool(
        &self,
        value: Rc<Value>,
        operator: &Token<'src>,
    ) -> Result<bool, RuntimeError<'src>> {
        value.as_bool().ok_or(RuntimeError::with_token(
            operator,
            "Operand must be a boolean",
        ))
    }

    fn evaluate(&mut self, expr: &Expr<'src>) -> ExprEvalResult<'src> {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: &Stmt<'src>) -> StmtEvalResult<'src> {
        stmt.accept(self)
    }

    fn execute_statements(&mut self, stmts: &[Stmt<'src>]) -> StmtEvalResult<'src> {
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

    fn eval<'src>(src: &'src str, var: &'src str) -> Result<Rc<Value<'src>>, RuntimeError<'src>> {
        let (toks, _) = Scanner::scan(src);
        let (stmts, _) = Parser::parse(toks);

        let mut interp = Interpreter::new();
        for stmt in stmts {
            interp.execute(&stmt)?;
        }

        let tok = Token::new(TokenType::Identifier, var, None, 0);
        let env = interp.environment.borrow();
        Ok(env.get(&tok).unwrap())
    }

    #[test]
    fn basic_expression_evaluation() {
        let cases = [
            ("x = 1 + 2;", Value::Number(3.0)),
            ("x = \"ab\" + \"cd\";", Value::String("abcd".into())),
            ("x = 5 * (3 + 4) % 3;", Value::Number(2.0)),
            ("x = !false;", Value::Boolean(true)),
        ];

        for (expr, expected) in cases {
            let got = eval(expr, "x").unwrap();
            assert_eq!(*got, expected, "evaluating `{}`", expr);
        }
    }

    #[test]
    fn logical_and_evaluation() {
        assert_eq!(
            *eval("x = true and true;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            *eval("x = true and false;", "x").unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            *eval("x = false and true;", "x").unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            *eval("x = false and false;", "x").unwrap(),
            Value::Boolean(false)
        );
    }

    #[test]
    fn logical_or_evaluation() {
        assert_eq!(
            *eval("x = true or true;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            *eval("x = true or false;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            *eval("x = false or true;", "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            *eval("x = false or false;", "x").unwrap(),
            Value::Boolean(false)
        );
    }

    #[test]
    fn logical_precedence() {
        assert_eq!(
            *eval("x = false or true and false;", "x").unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            *eval("x = true and false or true;", "x").unwrap(),
            Value::Boolean(true)
        );
    }

    #[test]
    fn logical_short_circuit_does_not_evaluate_rhs() {
        let a = eval("x = false and undefinedVar;", "x").unwrap();
        assert_eq!(*a, Value::Boolean(false));

        let b = eval("x = true or undefinedVar;", "x").unwrap();
        assert_eq!(*b, Value::Boolean(true));
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
        assert_eq!(*eval(src, "x").unwrap(), Value::Number(2.0));
    }

    #[test]
    fn if_true_executes_then_branch() {
        assert_eq!(
            *eval("x = 0; if true { x = 1; } else { x = 2; }", "x").unwrap(),
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
        assert_eq!(*eval(src, "x").unwrap(), Value::Number(2.0));
    }

    #[test]
    fn else_branch_executes_when_condition_false() {
        assert_eq!(
            *eval("x = 0; if false { x = 1; } else { x = 2; }", "x").unwrap(),
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
        assert_eq!(*eval(src, "counter").unwrap(), Value::Number(3.0));
    }

    #[test]
    fn while_condition_false_initially() {
        let src = r#"
        i = 0;
        while i > 0 { i = i - 1; }
    "#;
        assert_eq!(*eval(src, "i").unwrap(), Value::Number(0.0));
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
        assert_eq!(*eval(src, "sum").unwrap(), Value::Number(10.0));
    }

    #[test]
    fn function_call_add() {
        let src = r#"
            fn add(a, b) { return a + b; }
            result = add(2, 3);
        "#;
        assert_eq!(*eval(src, "result").unwrap(), Value::Number(5.0));
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
        assert_eq!(*eval(src, "eight").unwrap(), Value::Number(8.0));
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
            *eval(explicit_shadow_src, "global_val").unwrap(),
            Value::Number(300.0),
            "Global variable modified correctly from within a function that has shadowed parameters."
        );
        assert_eq!(
            *eval(explicit_shadow_src, "outer_shadow_var").unwrap(),
            Value::Number(50.0),
            "Outer variable should not be affected by assignment to a shadowed parameter in a different function."
        );
    }

    #[test]
    fn test_struct_declaration_and_instantiation_empty() {
        let src = "struct T {} x = T {};";
        let val = eval(src, "x").unwrap();
        match &*val {
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
        assert_eq!(*eval(src, "val_x").unwrap(), Value::Number(1.0));
        assert_eq!(*eval(src, "val_y").unwrap(), Value::Number(3.0));
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
        assert_eq!(*eval(src, "result").unwrap(), Value::Number(12.0));
    }

    #[test]
    fn test_struct_static_method_call() {
        let src = r#"
            struct MathUtils {
                fn add(a, b) { return a + b; }
            }
            sum = MathUtils.add(5, 7);
        "#;
        assert_eq!(*eval(src, "sum").unwrap(), Value::Number(12.0));
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
            .contains("All and only defined struct variables must be initialized."));
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
        assert!(err
            .message
            .contains("Attempted to call a non-callable value"));
    }

    #[test]
    fn error_init_missing_field() {
        let src = "struct Point {x,y} p = Point {x:1};";
        let err = eval(src, "p").unwrap_err();
        assert!(err
            .message
            .contains("All and only defined struct variables must be initialized."));
    }

    #[test]
    fn for_loop_rejects_float_bounds() {
        let err = eval("for i in 0.5..2 { }", "i").unwrap_err();
        assert!(err.message.contains("Expected integer."));

        let err = eval("for i in 0..2.5 { }", "i").unwrap_err();
        assert!(err.message.contains("Expected integer."));
    }

    #[test]
    fn string_ordering_works() {
        assert_eq!(
            *eval(r#"x = "a" <  "b";"#, "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            *eval(r#"x = "z" >= "y";"#, "x").unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            *eval(r#"x = "cat" > "car";"#, "x").unwrap(),
            Value::Boolean(true)
        );
    }

    #[test]
    fn mixed_type_ordering_errors() {
        let err = eval(r#"x = 1 < "b";"#, "x").unwrap_err();
        assert!(err.message.contains("two numbers or two strings"));
    }

    #[test]
    fn field_callable_not_treated_as_method() {
        let src = r#"
            struct Box { f }

            fn test(x) { return x + 1; }

            b = Box { f: test };
            res = b.f(3);
        "#;
        assert_eq!(*eval(src, "res").unwrap(), Value::Number(4.0));
    }
}
