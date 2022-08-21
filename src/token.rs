#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Variant {
    Illegal,
    EndOfFile,
    Identifier,
    Integer,
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
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Equals,
    NotEqual
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Token {
    pub variant: Variant,
    pub value: Option<String>
}

impl Token {
    pub fn new(variant: Variant) -> Token {
        Token {
            variant,
            value: None
        }
    }
    
    pub fn new_with_value(variant: Variant, value: &str) -> Token {
        Token {
            variant,
            value: Some(value.to_string())
        }
    }
}