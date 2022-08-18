mod lexer;
mod token;

use lexer::Lexer;
use token::Token;

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