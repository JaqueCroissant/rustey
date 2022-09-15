mod lexer;
mod token;
mod parser;
mod ast;
mod eval;

use std::collections::HashMap;
use lexer::Lexer;
use parser::Parser;
use eval::Object;

pub fn run(input: String){
    let mut environment: HashMap<String, Object> = HashMap::new();
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    println!("{:?}", program);

    if parser.errors.len() != 0 {
        print_errors(&parser.errors);
        return;
    }

    let evaluted_program = eval::evaluate(program, &mut environment);
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