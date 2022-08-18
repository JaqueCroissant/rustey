use super::token::Token;

pub struct Program {
    pub statements: Vec<Statement>
}

pub struct Statement{
    //kind: Kind,
    //identifier: String,
    //value: String
}

pub enum Kind{
    Let,
    Return,
    If
}
