mod lexer;
mod token;
mod ast;
mod parser;

use lexer::Lexer;
use token::Token;

//this is basically the REPL (Read, Eval, Print, Loop)

pub fn run(input: String){
    let mut lexer = Lexer::new(input);

    loop{
        let token = lexer.next_token();

        println!("{:?}", token);

        if token == Token::EndOfFile {
            break;
        }
    }
}