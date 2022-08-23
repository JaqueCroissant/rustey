use super::token::Variant;

#[derive(Debug, PartialEq, Clone)]
pub struct Expression{
    pub variant: ExprVariant,
    pub value: String
}

impl Expression {
    pub fn new(variant: ExprVariant, value: String) -> Expression {
        Expression { variant, value }
    }

    pub fn override_variant(&mut self, variant: ExprVariant) {
        self.variant = variant;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprVariant {
    Identifier,
    Infix(Infix),
    Integer,
    Prefix(Prefix)
}

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
    LT,
    GT,
    Equals,
    NotEquals,
}