mod lexer;
mod token;
mod parser;
mod ast;

use lexer::Lexer;
use parser::Parser;

//this is basically the REPL (Read, Eval, Print, Loop)

pub fn run(input: String){
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
}