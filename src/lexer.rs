use std::char;
use super::token::Token;

pub struct Lexer{
    input: String,
    position: usize,
    read_position: usize,
    current_char: Option<char>
}

impl Lexer{
    pub fn new (input: String) -> Lexer {
        Lexer { 
            input: input, 
            position: 0, 
            read_position: 0, 
            current_char: None
        }
    }
    
    pub fn next_token(&mut self) -> Token {
        self.read_char();
        self.skip_whitespace();

        if self.current_char == None {
            return Token::EndOfFile;
        }

        let result = match self.current_char.unwrap() {
            '=' => Token::Assign,
            '+' => Token::Plus,
            '(' => Token::LeftParentheses,
            ')' => Token::RightParentheses,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon, 
            _ => Token::Illegal
        };

        result
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = None;
        }
        else{
            self.current_char = self.input.chars().nth(self.read_position);
            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    fn skip_whitespace(&mut self) {

        if self.current_char == None {
            return;
        }

        loop {
            match self.current_char.unwrap() {
                  ' ' 
                | '\t' 
                | '\n' 
                | '\r' => self.read_char(),
                _ => break,
            }
        }
    }
}

#[cfg(test)]
#[test]
fn can_parse_string() {

    let input = String::from("=+(){},;^");

    let mut sut = Lexer::new(input);

    let expected_results = [
        Token::Assign, 
        Token::Plus,
        Token::LeftParentheses,
        Token::RightParentheses,
        Token::LeftBrace,
        Token::RightBrace,
        Token::Comma,
        Token::Semicolon,
        Token::Illegal,
        Token::EndOfFile
        ];

    for expected in expected_results {
        let actual = sut.next_token();
        assert_eq!(actual, expected);
    }
}

#[test]
fn can_parse_simple_program() {

    let program = 
    "let five = 5;
    let ten = 10;
    
    let add = fn(x, y) {
        x + y;
    };
    
    let result = add(five, ten);
    ";

    let input = String::from(program);
    let mut sut = Lexer::new(input);

    let expected_results = [
        Token::Let, 
        Token::Identifier(String::from("five")),
        Token::Assign,
        Token::Integer(5),
        Token::Semicolon,
        Token::Let, 
        Token::Identifier(String::from("ten")),
        Token::Assign,
        Token::Integer(10),
        Token::Semicolon,
        Token::Let, 
        Token::Identifier(String::from("add")),
        Token::Assign,
        Token::Function,
        Token::LeftParentheses,
        Token::Identifier(String::from("x")),
        Token::Comma,
        Token::Identifier(String::from("y")),
        Token::RightParentheses,
        Token::LeftBrace,
        Token::Identifier(String::from("x")),
        Token::Plus,
        Token::Identifier(String::from("y")),
        Token::Semicolon,
        Token::RightBrace,
        Token::Semicolon,
        Token::Let, 
        Token::Identifier(String::from("result")),
        Token::Assign,
        Token::Identifier(String::from("add")),
        Token::LeftParentheses,
        Token::Identifier(String::from("five")),
        Token::Comma,
        Token::Identifier(String::from("ten")),
        Token::RightParentheses,
        Token::Semicolon,
        Token::EndOfFile
        ];

    for expected in expected_results {
        let actual = sut.next_token();
        assert_eq!(actual, expected);
    }
}