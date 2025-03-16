mod ast;
mod frontend;
mod interpreter;
mod neu;
mod utils;

use std::env;

use ast::expr::Expr;
use frontend::literal::Literal;
use frontend::token::Token;
use neu::Neu;
use utils::ast_printer::AstPrinter;

fn main() {
    let args: Vec<String> = env::args().collect();
    Neu::interpret(args);

    let expr = Expr::Binary {
        left: Box::new(Expr::Unary {
            operator: Token::new("-".into(), None, 1),
            right: Box::new(Expr::Literal(Literal::Number(123.0))),
        }),
        operator: Token::new("*".into(), None, 1),
        right: Box::new(Expr::Grouping {
            expression: Box::new(Expr::Literal(Literal::Boolean(true))),
        }),
    };

    let mut ast_printer = AstPrinter;
    println!("{}", ast_printer.print(&expr));
}
