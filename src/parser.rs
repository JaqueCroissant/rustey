use super::ast::{Expression, ExprVariant, Statement};
use super::lexer::Lexer;
use super::token::{Token,Variant};
use std::collections::HashMap;

const LOWEST: i32 = 1;
const EQUALS: i32 = 2;
const LESSGREATER: i32 = 3;
const SUM: i32 = 4;
const PRODUCT: i32 = 5;
const PREFIX: i32 = 6;
const CALL: i32 = 7;

pub struct Program {
    pub statements: Vec<Statement>
}

impl Program {
    fn new() -> Program {
        return Program { statements: vec![] }
    }
}

pub struct Parser{
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        
        let mut parser = Parser { 
            lexer, 
            current_token: Token::new(Variant::Illegal), 
            peek_token: Token::new(Variant::Illegal),
            errors: vec![]
        };

        //parser.register_prefix_function(Variant::Identifier, &parser.infix_parse_function);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, variant: Variant) -> bool{
        if self.peek_token_is(&variant){
            self.next_token();
            return true;
        }
        self.peek_error(&variant);
        false
    }

    fn current_token_is(&self, variant: &Variant) -> bool{
        &self.current_token.variant == variant
    }

    fn peek_token_is(&self, variant: &Variant) -> bool{
        &self.peek_token.variant == variant
    }

    fn peek_error(&mut self, token: &Variant){
        let message = format!("expected {:?} but got {:?}", token, self.peek_token);
        self.errors.push(message);
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token.variant != Variant::EndOfFile {
            let statement = self.parse_statement();

            match statement {
                Some(x) => program.statements.push(x),
                None => (),
            }
            
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let result = match self.current_token.variant {
            Variant::Let => self.parse_let_statement(),
            Variant::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement()
        };

        result
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        
        if !self.expect_peek(Variant::Identifier) {
            return None;
        };

        let value = match &self.current_token.value { 
            Some(x) => x.clone(),
            _ => panic!()
        };

        if !self.expect_peek(Variant::Assign) {
          return None;
        };

        let expression = Expression::new(ExprVariant::Identifier, value);
        let statement = Statement::new(Variant::Let, Some(expression));

        while self.current_token.variant == Variant::Semicolon {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let expression = Expression::new(ExprVariant::Identifier, "".to_string()); // FIX ME
        let statement = Statement::new(Variant::Return, Some(expression));

        self.next_token();


        while self.current_token.variant != Variant::Semicolon {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(LOWEST);

        if expression == None {
            return None;
        }

        let statement = Statement::new(self.current_token.variant.clone(), expression);

        if self.peek_token_is(&Variant::Semicolon){
            self.next_token();
        }

        Some(statement)
    }

    pub fn parse_identifier(&mut self) -> Option<Expression> {
        let result = match &self.current_token.value {
            Some(x) => x.clone(),
            _ => panic!()
        };

        Some(Expression::new(ExprVariant::Identifier, result.clone()))
    }

    fn parse_expression(&mut self, precedence: i32) -> Option<Expression>{

        match &self.current_token.variant {
            Variant::Identifier => return self.parse_identifier(),
            _ => return None
        };
    }

    fn prefix_parse_function(&mut self) -> Option<Expression>{
        None
    }

    fn infix_parse_function(&mut self, expression: Expression) -> Option<Expression>{
        None
    }

    
}

// AT PAGE 57

#[cfg(test)]
#[test]
fn let_statements() {
    let input = "
    let dingus = 5;
    let y = 10;
    let foobar 838383;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected_identifiers = [
        "dingus".to_string(), 
        "y".to_string()
    ];

    assert_eq!(parser.errors.len(), 1);
    assert_eq!(program.statements.len(), 2);
    
    for (i, _el) in expected_identifiers.iter().enumerate() {
        let statement = &program.statements[i];
        
        assert_eq!(statement.token_variant, Variant::Let);
        assert_eq!(statement.expression.as_ref().unwrap().variant, ExprVariant::Identifier);
        assert_eq!(statement.expression.as_ref().unwrap().value, expected_identifiers[i]);
    }
}

#[test]
fn return_statements() {
    let input = "
    return 5;
    return 10;
    return 838383;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 3);
    
    for s in program.statements {
        assert_eq!(s.token_variant, Variant::Return);
    }
}

#[test]
fn identifier_expressions() {
    let input = "
    foobar;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 1);
    
    for s in program.statements {
        assert_eq!(s.token_variant, Variant::Identifier);
        assert_ne!(s.expression, None);
    }
}