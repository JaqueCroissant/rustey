mod lexer;
mod token;
mod parser;
mod ast;
mod eval;

use lexer::Lexer;
use parser::{Parser, Program};

//this is basically the REPL (Read, Eval, Print, Loop)

pub fn run(input: String){
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.errors.len() != 0 {
        print_errors(&parser.errors);
    }

    print_program(&program);
}

fn print_errors(errors: &Vec<String>){
    println!("Rustey encountered the following errors while parsing your program:");
    for error in errors {
        println!("{}", error);    
    }
    println!("");
}

fn print_program(program: &Program) {
    for statement in program.statements.clone() {
        println!("{:?}", statement);   
    }
}