mod lexer;
mod token;
mod parser;
mod ast;
mod eval;
mod environment;

use lexer::Lexer;
use parser::Parser;
use eval::Object;
use environment::Environment;
use std::rc::Rc;

pub fn run(input: String){
    println!("{:?}", input);
    let environment = Environment::new();
    let lexer = Lexer::new(r#"let message = "some" + " " + "text""#.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.errors.len() != 0 {
        print_errors(&parser.errors);
        return;
    }

    println!("program is {:?}", program);
    let evaluted_program = eval::evaluate(program, &mut Rc::new(environment));
    print_program(&evaluted_program);
}

fn print_errors(errors: &Vec<String>){
    println!("Rustey encountered the following errors while parsing your program:");
    for error in errors {
        println!("{}", error);    
    }
}

fn print_program(objects: &Vec<Object>) {
    for o in objects {
        println!("{:?}", eval::inspect(o));   
    }
}