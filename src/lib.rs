mod lexer;
mod token;
mod parser;
mod ast;
mod eval;

use lexer::Lexer;
use parser::Parser;
use eval::Object;

//this is basically the REPL (Read, Eval, Print, Loop)

pub fn run(input: String){
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.errors.len() != 0 {
        print_errors(&parser.errors);
        //return;
    }
    println!("{:?}", program);
    let evaluted_program = eval::evaluate_program(program);
    print_program(&evaluted_program);
}

fn print_errors(errors: &Vec<String>){
    println!("Rustey encountered the following errors while parsing your program:");
    for error in errors {
        println!("{}", error);    
    }
    println!("");
}

fn print_program(objects: &Vec<Object>) {
    for o in objects {
        println!("{:?}", eval::inspect(o));   
    }
}