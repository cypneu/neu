use crate::frontend::parser::Parser;
use crate::frontend::scanner::Scanner;
use std::fs;
use std::io::{self, Write};

#[derive(Debug)]
pub struct Neu {
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

    pub fn report(&mut self, line: usize, place: String, message: String) {
        eprintln!("[line {}] Error{}: {}", line, place, message);
        self.had_error = true;
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

        let ast = Parser::parse(tokens, self);

        if self.had_error {
            return;
        }

        println!("{:?}", ast);
    }

    fn new() -> Self {
        Neu { had_error: false }
    }
}
