use crate::frontend::parser::Parser;
use crate::frontend::scanner::Scanner;
use crate::interpreter::Interpreter;
use crate::utils::ast_printer::AstPrinter;
use std::fs;
use std::io::{self, Write};

#[derive(Debug)]
pub struct Neu {
    had_error: bool,
}

impl Neu {
    pub fn new() -> Self {
        Neu { had_error: false }
    }

    pub fn report(&mut self, line: usize, place: String, message: String) {
        eprintln!("[line {}] Error{}: {}", line, place, message);
        self.had_error = true;
    }

    pub fn run_file(&mut self, path: &String) {
        let contents = fs::read_to_string(path).unwrap();
        self.run(contents);
        if self.had_error {
            std::process::exit(65);
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
        for token in &tokens {
            println!("{:?}", token);
        }

        let ast = Parser::parse(tokens, self);

        if self.had_error {
            return;
        }

        let literal = ast.accept(&mut Interpreter);
        println!("{:?}", literal);

        let mut ast_printer = AstPrinter;
        println!("{}", ast_printer.print(&ast));
    }
}
