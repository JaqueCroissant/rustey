use super::token::Variant;

#[derive(Debug, PartialEq)]
pub struct Expression{
    pub variant: ExprVariant,
    pub value: String,
    pub operator: Option<String>
}

impl Expression {
    pub fn new(variant: ExprVariant, value: String) -> Expression {
        Expression { variant, value, operator: None }
    }

    pub fn new_with_operator(variant: ExprVariant, value: String, operator: String) -> Expression {
        Expression { variant, value, operator: Some(operator) }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprVariant {
    Identifier,
    Infix,
    Integer,
    Prefix
}

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