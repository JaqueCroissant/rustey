#[derive(Debug, PartialEq, Eq)]
pub enum Token{
    Illegal,
    EndOfFile,
    Identifier(String),
    Integer(i32),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LeftParentheses,
    RightParentheses,
    LeftBrace,
    RightBrace,
    Function,
    Let
}