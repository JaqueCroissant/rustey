use super::token::Variant;

#[derive(Debug, Clone)]
pub struct Statement{
    pub token_variant: Variant,
    pub expression: Option<Expression>
}

impl Statement {
    pub fn new(token_variant: Variant, expression: Option<Expression>) -> Statement {
        let statement = Statement { 
            token_variant,
            expression: expression
            };

        statement
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    Infix(Box<Expression>, Infix, Box<Expression>),
    Integer(String),
    Prefix(Prefix, Box<Expression>),
    Bool(bool)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Minus,
    Bang
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equals,
    NotEqual,
}