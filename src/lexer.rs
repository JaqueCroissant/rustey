use std::char;
use super::token::{Token, Variant};

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
            return Token::new(Variant::EndOfFile);
        }

        let result = match self.current_char.unwrap() {
            x if self.is_identifier(x) => return self.read_identifier(),
            x if x.is_numeric() => return self.read_integer(),
            '"' => self.read_string(),
            '=' => self.assign_or_equals(),
            '!' => self.bang_or_not_equal(),
            '+' => Token::new(Variant::Plus),
            '-' => Token::new(Variant::Minus),
            '(' => Token::new(Variant::LeftParentheses),
            ')' => Token::new(Variant::RightParentheses),
            '{' => Token::new(Variant::LeftBrace),
            '}' => Token::new(Variant::RightBrace),
            ',' => Token::new(Variant::Comma),
            ';' => Token::new(Variant::Semicolon),
            '*' => Token::new(Variant::Asterisk),
            '/' => Token::new(Variant::Slash),
            '<' => Token::new(Variant::LessThan),
            '>' => Token::new(Variant::GreaterThan),
            _ => Token::new(Variant::Illegal)
        };

        self.read_char();

        result
    }

    fn read_string(&mut self) -> Token {

        let position = self.position + 1;

        loop {
            self.read_char();

            match self.current_char {
                None => break,
                Some(x) => {
                    if x == '"' {
                        break;
                    }
                } 
            }
        }

        Token::new_with_value(Variant::String, &self.input[position..self.position])
    }

    fn assign_or_equals(&mut self) -> Token {
        let peek = self.peek_char();
                
        if peek != None && peek.unwrap() == '=' {
            self.read_char();
            return Token::new(Variant::Equals);
        }
        
        Token::new(Variant::Assign)
    }

    fn bang_or_not_equal(&mut self) -> Token {
        let peek = self.peek_char();
                
        if peek != None && peek.unwrap() == '=' {
            self.read_char();
            return Token::new(Variant::NotEqual);
        }

        return Token::new(Variant::Bang);
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
        
        return self.input.chars().nth(self.read_position)
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
            "fn" => Token::new(Variant::Function),
            "let" => Token::new(Variant::Let),
            "if" => Token::new(Variant::If),
            "else" => Token::new(Variant::Else),
            "return" => Token::new(Variant::Return),
            "true" | "false" => Token::new_with_value(Variant::Bool, x),
            _ => Token::new_with_value(Variant::Identifier, x),
        };

        token
    }

    fn read_integer(&mut self) -> Token
    {
        let start = self.position;

        while self.current_char != None && self.current_char.unwrap().is_numeric() {
            self.read_char();
        }

        let num = &self.input[start..self.position];
        Token::new_with_value(Variant::Integer, num)
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
        Variant::Assign, 
        Variant::Plus,
        Variant::LeftParentheses,
        Variant::RightParentheses,
        Variant::LeftBrace,
        Variant::RightBrace,
        Variant::Comma,
        Variant::Semicolon,
        Variant::Illegal,
        Variant::EndOfFile
        ];

    for expected in expected_results {
        let actual = sut.next_token();
        assert_eq!(actual.variant, expected);
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
    \"foobar\"
    \"foo bar\"
    ";

    let input = String::from(program);
    let mut sut = Lexer::new(input);

    let expected_results = [
        Token::new(Variant::Let), 
        Token::new_with_value(Variant::Identifier, "five"),
        Token::new(Variant::Assign),
        Token::new_with_value(Variant::Integer, "5"),
        Token::new(Variant::Semicolon),
        Token::new(Variant::Let), 
        Token::new_with_value(Variant::Identifier, "ten"),
        Token::new(Variant::Assign),
        Token::new_with_value(Variant::Integer,"10"),
        Token::new(Variant::Semicolon),
        Token::new(Variant::Let), 
        Token::new_with_value(Variant::Identifier, "add"),
        Token::new(Variant::Assign),
        Token::new(Variant::Function),
        Token::new(Variant::LeftParentheses),
        Token::new_with_value(Variant::Identifier, "x"),
        Token::new(Variant::Comma),
        Token::new_with_value(Variant::Identifier, "y"),
        Token::new(Variant::RightParentheses),
        Token::new(Variant::LeftBrace),
        Token::new_with_value(Variant::Identifier, "x"),
        Token::new(Variant::Plus),
        Token::new_with_value(Variant::Identifier, "y"),
        Token::new(Variant::Semicolon),
        Token::new(Variant::RightBrace),
        Token::new(Variant::Semicolon),
        Token::new(Variant::Let), 
        Token::new_with_value(Variant::Identifier, "result"),
        Token::new(Variant::Assign),
        Token::new_with_value(Variant::Identifier, "add"),
        Token::new(Variant::LeftParentheses),
        Token::new_with_value(Variant::Identifier, "five"),
        Token::new(Variant::Comma),
        Token::new_with_value(Variant::Identifier, "ten"),
        Token::new(Variant::RightParentheses),
        Token::new(Variant::Semicolon),
        Token::new(Variant::Bang),
        Token::new(Variant::Minus),
        Token::new(Variant::Slash),
        Token::new(Variant::Asterisk),
        Token::new_with_value(Variant::Integer, "5"),
        Token::new(Variant::Semicolon),
        Token::new_with_value(Variant::Integer, "5"),
        Token::new(Variant::LessThan),
        Token::new_with_value(Variant::Integer, "10"),
        Token::new(Variant::GreaterThan),
        Token::new_with_value(Variant::Integer, "5"),
        Token::new(Variant::Semicolon),
        Token::new(Variant::If),
        Token::new(Variant::LeftParentheses),
        Token::new_with_value(Variant::Integer, "5"),
        Token::new(Variant::LessThan),
        Token::new_with_value(Variant::Integer, "10"),
        Token::new(Variant::RightParentheses),
        Token::new(Variant::LeftBrace),
        Token::new(Variant::Return),
        Token::new_with_value(Variant::Bool, "true"),
        Token::new(Variant::Semicolon),
        Token::new(Variant::RightBrace),
        Token::new(Variant::Else),
        Token::new(Variant::LeftBrace),
        Token::new(Variant::Return),
        Token::new_with_value(Variant::Bool, "false"),
        Token::new(Variant::Semicolon),
        Token::new(Variant::RightBrace),
        Token::new_with_value(Variant::Integer, "10"),
        Token::new(Variant::Equals),
        Token::new_with_value(Variant::Integer, "10"),
        Token::new(Variant::Semicolon),
        Token::new_with_value(Variant::Integer, "10"),
        Token::new(Variant::NotEqual),
        Token::new_with_value(Variant::Integer, "9"),
        Token::new(Variant::Semicolon),
        Token::new_with_value(Variant::String, "foobar"),
        Token::new_with_value(Variant::String, "foo bar"),
        Token::new(Variant::EndOfFile)
        ];

    for expected in expected_results {
        let actual = sut.next_token();
        assert_eq!(actual, expected);
    }
}