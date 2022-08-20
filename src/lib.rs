mod lexer;
mod token;
mod parser;

use lexer::Lexer;
use parser::Parser;

//this is basically the REPL (Read, Eval, Print, Loop)

pub fn run(input: String){
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
}