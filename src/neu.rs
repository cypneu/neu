use crate::frontend::parser::Parser;
use crate::frontend::scanner::Scanner;
use crate::interpreter::Interpreter;
use crate::runtime_error::RuntimeError;
use crate::utils::ast_printer::AstPrinter;
use std::fs;
use std::io::{self, Write};

#[derive(Debug)]
pub struct Neu {
    had_error: bool,
    had_runtime_error: bool,
}

impl Neu {
    pub fn new() -> Self {
        Neu {
            had_error: false,
            had_runtime_error: false,
        }
    }

    pub fn report(&mut self, line: usize, place: String, message: String) {
        eprintln!("\x1b[31mError{} [Line {}]\x1b[0m: {}", place, line, message);
        self.had_error = true;
    }

    pub fn runtime_error(&mut self, error: RuntimeError) {
        eprintln!(
            "\x1b[31mRuntime Error [Line {}]\x1b[0m: {}",
            error.token.line, error.message
        );
        self.had_runtime_error = true;
    }

    pub fn run_file(&mut self, path: &String) {
        let contents = fs::read_to_string(path).unwrap();
        self.run(contents);
        if self.had_error {
            std::process::exit(65);
        }
        if self.had_runtime_error {
            std::process::exit(70);
        }
    }

    pub fn run_prompt(&mut self) {
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
        let ast = match Parser::parse(tokens, self) {
            Some(ast) => ast,
            None => return,
        };

        if self.had_error {
            return;
        }

        match Interpreter.interpret(&ast) {
            Ok(value) => println!("{:?}", value),
            Err(err) => self.runtime_error(err),
        }

        // let mut ast_printer = AstPrinter;
        // println!("{}", ast_printer.print(&ast));
    }
}
