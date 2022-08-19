use std::char;
use std::str::FromStr;
use super::token::Token;

pub struct Lexer{
    input: String,
    position: usize,
    read_position: usize,
    current_char: Option<char>
}

impl Lexer{
    pub fn new (input: String) -> Lexer {
        let mut lexer = Lexer { 
            input: input, 
            position: 0, 
            read_position: 0, 
            current_char: None
        };

        lexer.read_char();

        return lexer;
    }
    
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.current_char == None {
            return Token::EndOfFile;
        }

        let result = match self.current_char.unwrap() {
            x if self.is_identifier(x) => return self.read_identifier(),
            x if x.is_numeric() => return self.read_integer(),
            '=' => self.assign_or_equals(),
            '!' => self.bang_or_not_equal(),
            '+' => Token::Plus,
            '-' => Token::Minus,
            '(' => Token::LeftParentheses,
            ')' => Token::RightParentheses,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            _ => Token::Illegal
        };

        self.read_char();

        result
    }

    fn assign_or_equals(&mut self) -> Token {
        let peek = self.peek_char();
                
        if peek != None && peek.unwrap() == '=' {
            self.read_char();
            return Token::Equals
        }
        else{
            return Token::Assign;
        }
    }

    fn bang_or_not_equal(&mut self) -> Token {
        let peek = self.peek_char();
                
        if peek != None && peek.unwrap() == '=' {
            self.read_char();
            return Token::NotEqual
        }
        else{
            return Token::Bang;
        }
    }

    fn read_char(&mut self) {
        if self.read_position > self.input.len() {
            self.current_char = None;
        }
        else{
            self.current_char = self.input.chars().nth(self.read_position);
            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        if self.read_position > self.input.len() {
            return None;
        }
        else{
            return self.input.chars().nth(self.read_position);
        }
    }

    fn skip_whitespace(&mut self) {

        while self.current_char != None{
            match self.current_char.unwrap() {
                  ' ' 
                | '\t' 
                | '\n' 
                | '\r' => self.read_char(),
                _ => break,
            }
        }
    }

    fn read_identifier(&mut self) -> Token
    {
        let start = self.position;

        while self.current_char != None && self.is_identifier(self.current_char.unwrap()) {
            self.read_char();
        }

        let x = &self.input[start..self.position];
        let token = match x {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Identifier(x.to_string()),
        };

        token
    }

    fn read_integer(&mut self) -> Token
    {
        let start = self.position;

        while self.current_char != None && self.current_char.unwrap().is_numeric() {
            self.read_char();
        }

        let num: i32 = FromStr::from_str(&self.input[start..self.position]).unwrap();
        Token::Integer(num)
    }

    fn is_identifier(&mut self, input: char) -> bool {
        match input {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false
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
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
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
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Integer(5),
        Token::Semicolon,
        Token::Integer(5),
        Token::LessThan,
        Token::Integer(10),
        Token::GreaterThan,
        Token::Integer(5),
        Token::Semicolon,
        Token::If,
        Token::LeftParentheses,
        Token::Integer(5),
        Token::LessThan,
        Token::Integer(10),
        Token::RightParentheses,
        Token::LeftBrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::RightBrace,
        Token::Else,
        Token::LeftBrace,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::RightBrace,
        Token::Integer(10),
        Token::Equals,
        Token::Integer(10),
        Token::Semicolon,
        Token::Integer(10),
        Token::NotEqual,
        Token::Integer(9),
        Token::Semicolon,
        Token::EndOfFile
        ];

    for expected in expected_results {
        let actual = sut.next_token();
        assert_eq!(actual, expected);
    }
}