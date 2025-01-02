mod scanner;
mod token;

use scanner::Scanner;
use std::env;
use std::fs;
use std::io::{self, Write};

#[derive(Debug)]
struct Neu {
    had_error: bool,
}

impl Neu {
    fn new() -> Self {
        Neu { had_error: false }
    }

    pub fn main(args: Vec<String>) {
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
        let mut scanner = Scanner::new(&source, self);
        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{:?}", token);
        }
    }

    pub fn error(&mut self, line: usize, message: String) {
        self.report(line, "".into(), message);
    }

    fn report(&mut self, line: usize, place: String, message: String) {
        eprintln!("[line {}] Error{}: {}", line, place, message);
        self.had_error = true;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    Neu::main(args);
}
