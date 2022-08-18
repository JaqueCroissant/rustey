use std::mem;

use crate::ast::Statement;
use super::lexer::Lexer;
use super::token::Token;
use super::ast::Program;

pub struct Parser{
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser{
        
        let mut parser = Parser { 
            lexer, 
            current_token: None, 
            peek_token: None 
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = Some(self.lexer.next_token());
    }

    pub fn parse_program(&self) -> Program {
        Program { statements: vec![Statement{}] }
    }
}

#[cfg(test)]
#[test]
fn let_statements() {
    let input = "
    let x = 5;
    let y = 10;
    let foobar = 838383;
    ".to_string();

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);

    let program = parser.parse_program();

    assert_eq!(program.statements.len(), 3);
}