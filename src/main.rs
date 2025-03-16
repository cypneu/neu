mod ast_printer;
mod expr;
mod neu;
mod parser;
mod scanner;
mod token;
mod visitor;
use std::env;

use ast_printer::AstPrinter;
use expr::{Expr, Value};
use neu::Neu;
use token::{Literal, Token};

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
