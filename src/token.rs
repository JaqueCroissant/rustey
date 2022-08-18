#[derive(Debug, PartialEq, Eq)]
pub enum Token{
    Illegal,
    EndOfFile,
    Identifier(String),
    Integer(i32),
    Assign,
    Plus,
    Minus,
    Comma,
    Semicolon,
    LeftParentheses,
    RightParentheses,
    LeftBrace,
    RightBrace,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Function,
    Let
}