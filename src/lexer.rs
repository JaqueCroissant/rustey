use super::token::Token;

pub struct Lexer{
    input: String
}

impl Lexer{
    pub fn new (input: String) -> Lexer {
        Lexer { input }
    }
    
    pub fn next_token(&self) -> Token{
        Token::EndOfFile
    }
}

#[cfg(test)]
#[test]
fn next_token_works() {

    let input = String::from("=+(){},;^");

    let sut = Lexer::new(input);

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