use super::token::Variant;

#[derive(Debug, Clone, PartialEq)]
pub struct Statement{
    pub variant: Variant,
    pub expression: Option<Expression>
}

impl Statement {
    pub fn new(variant: Variant, expression: Option<Expression>) -> Statement {
        let statement = Statement { 
            variant: variant,
            expression: expression
            };

        statement
    }
}

#[derive(Debug, Clone,PartialEq)]
pub struct BlockStatement{
    pub variant: Variant,
    pub statements: Vec<Statement>
}

impl BlockStatement {
    pub fn new(variant: Variant) -> BlockStatement {
        let statement = BlockStatement { 
            variant: variant,
            statements: vec![]
            };
            
        statement
    }

    pub fn new_with_statements(variant: Variant, statements: Vec<Statement>) -> BlockStatement {
        let statement = BlockStatement { 
            variant: variant,
            statements: statements
            };
            
        statement
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    Infix(Box<Expression>, Infix, Box<Expression>),
    Integer(i32),
    Prefix(Prefix, Box<Expression>),
    Bool(bool),
    If(Box<Expression>, BlockStatement),
    IfElse(Box<Expression>, BlockStatement, BlockStatement)
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