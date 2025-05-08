mod ast;
mod frontend;
mod neu;
mod runtime;

use neu::Neu;

use std::env;

fn main() {
    let mut neu = Neu::new();

    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => neu.run_prompt(),
        2 => neu.run_file(&args[1]),
        _ => {
            println!("Usage: neu [script]");
            std::process::exit(64)
        }
    }
}
