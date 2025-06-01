// use crate::ast::printer::AstPrinter;
use crate::frontend::parser::Parser;
use crate::frontend::scanner::Scanner;
use crate::runtime::interpreter::Interpreter;
use crate::runtime::runtime_error::RuntimeError;
use std::fs;
use std::io::{self, Write};

#[derive(Debug)]
pub struct Neu {
    pub had_error: bool,
    had_runtime_error: bool,
}

impl Neu {
    pub fn new() -> Self {
        Neu {
            had_error: false,
            had_runtime_error: false,
        }
    }

    pub fn report(&mut self, line: usize, place: impl Into<String>, message: String) {
        eprintln!(
            "\x1b[31mError{} [Line {}]\x1b[0m: {}",
            place.into(),
            line,
            message
        );
        self.had_error = true;
    }

    pub fn runtime_error(&mut self, RuntimeError { token, message }: RuntimeError) {
        let loc = token.map_or_else(String::new, |t| format!(" [Line {}]", t.line));
        eprintln!("\x1b[31mRuntime Error{}\x1b[0m: {}", loc, message);
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
        let (tokens, scan_errors) = Scanner::scan(&source);
        for error in scan_errors {
            self.report(error.line, "", error.message);
        }

        let (statements, parse_errors) = Parser::parse(tokens);
        for error in parse_errors {
            self.report(error.token.line, error.location, error.message);
        }

        if self.had_error {
            return;
        }

        Interpreter::interpret(statements);

        // let mut ast_printer = AstPrinter;
        // println!("{}", ast_printer.print(&ast));
    }
}
