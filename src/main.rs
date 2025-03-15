mod ast_printer;
mod expr;
mod parser;
mod scanner;
mod token;
mod visitor;

use ast_printer::AstPrinter;
use expr::{Expr, Value};
use parser::Parser;
use scanner::Scanner;
use std::env;
use std::fs;
use std::io::{self, Write};
use token::{Literal, Token};

#[derive(Debug)]
struct Neu {
    had_error: bool,
}

impl Neu {
    pub fn interpret(args: Vec<String>) {
        let mut neu = Neu::new();

        match args.len() {
            1 => neu.run_prompt(),
            2 => neu.run_file(&args[1]),
            _ => {
                println!("Usage: neu [script]");
                std::process::exit(64)
            }
        }
    }

    fn run_file(&mut self, path: &String) {
        let contents = fs::read_to_string(path).unwrap();
        self.run(contents);
        if self.had_error {
            std::process::exit(65);
        }
    }

    fn run_prompt(&mut self) {
        loop {
            print!("> ");
            let _ = io::stdout().flush();

            let mut line = String::new();
            let bytes_read = io::stdin().read_line(&mut line).unwrap();

            if bytes_read == 0 {
                break;
            }

            self.run(line);
            self.had_error = false;
        }
    }

    fn run(&mut self, source: String) {
        let tokens = Scanner::scan(&source, self);
        for token in &tokens {
            println!("{:?}", token);
        }

        let ast = Parser::parse(tokens);
        println!("{:?}", ast);
    }

    pub fn error(&mut self, line: usize, message: String) {
        self.report(line, "".into(), message);
    }

    fn report(&mut self, line: usize, place: String, message: String) {
        eprintln!("[line {}] Error{}: {}", line, place, message);
        self.had_error = true;
    }

    fn new() -> Self {
        Neu { had_error: false }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    Neu::interpret(args);

    let expr = Expr::Binary {
        left: Box::new(Expr::Unary {
            operator: Token::new("-".into(), None, 1),
            right: Box::new(Expr::Literal(Value::Literal(Literal::Number(123.0)))),
        }),
        operator: Token::new("*".into(), None, 1),
        right: Box::new(Expr::Grouping {
            expression: Box::new(Expr::Literal(Value::True)),
        }),
    };

    let mut ast_printer = AstPrinter;
    println!("{}", ast_printer.print(&expr));
}
