use crate::ast::BlockStatement;

use super::ast::{Expression, Statement, Prefix, Infix};
use super::lexer::Lexer;
use super::token::{Token,Variant};

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
    Equals,
    LessOrGreater,
    Sum,
    Product,
    Prefix,
    Call
}

#[derive(Debug)]
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
            errors: vec![],
        };

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

    fn peek_token_is(&self, variant: &Variant) -> bool{
        &self.peek_token.variant == variant
    }

    fn peek_error(&mut self, token: &Variant){
        let message = format!("expected {:?} but got {:?}", token, self.peek_token);
        self.errors.push(message);
    }

    fn peek_precedence(&mut self) -> Precedence {
        self.precedence(&self.peek_token.variant)
    }

    fn current_precedence(&mut self) -> Precedence {
        self.precedence(&self.current_token.variant)
    }

    fn precedence(&self, variant: &Variant) -> Precedence {
        match variant {
            Variant::Plus | Variant::Minus => Precedence::Sum,
            Variant::LessThan | Variant::GreaterThan => Precedence::LessOrGreater,
            Variant::Slash | Variant::Asterisk => Precedence::Product,
            Variant::Equals | Variant::NotEqual => Precedence::Equals,
            _ => Precedence::Lowest,
        }
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
            _ => {
                let message = format!("expected identifier but got {:?}", self.peek_token);
                self.errors.push(message);
                return None;
            }
        };

        if !self.expect_peek(Variant::Assign) {
          return None;
        };

        let expression = Expression::Identifier(value);
        let statement = Statement::new(Variant::Let, Some(expression));

        while self.current_token.variant == Variant::Semicolon {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let expression = Expression::Identifier("".to_string()); // TODO: FIX ME
        let statement = Statement::new(Variant::Return, Some(expression));

        self.next_token();

        while self.current_token.variant != Variant::Semicolon {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let current_token = self.current_token.variant.clone();
        let expression = self.parse_expression(Precedence::Lowest);
        
        let statement = Statement::new(current_token, Some(expression));

        if self.peek_token_is(&Variant::Semicolon){
            self.next_token();
        }

        Some(statement)
    }

    fn parse_bool(&mut self) -> Option<bool> {

        if self.current_token.value == None {
            let message = format!("expected boolean but found no value");
            self.errors.push(message);
            return None;
        }

        match self.current_token.value.clone().unwrap().parse::<bool>() {
            Ok(x) => return Some(x),
            Err(x) => {
                let message = format!("failed to parse bool; {:?}", x);
                self.errors.push(message);
                return None;
            }
        };
    }

    fn parse_integer(&mut self) -> Option<i32> {

        if self.current_token.value == None {
            let message = format!("expected integer but found no value");
            self.errors.push(message);
            return None;
        }

        match self.current_token.value.clone().unwrap().parse::<i32>() {
            Ok(x) => return Some(x),
            Err(x) => {
                let message = format!("failed to parse integer; {:?}", x);
                self.errors.push(message);
                return None;
            }
        };
    }

    fn parse_group(&mut self) -> Option<Expression>{
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Variant::RightParentheses){
            return None;
        }

        Some(expression)
    }

    fn parse_block(&mut self) -> BlockStatement {
        let mut block = BlockStatement::new(Variant::LeftBrace);

        self.next_token();

        while self.current_token.variant != Variant::RightBrace && 
              self.current_token.variant != Variant::EndOfFile {
            
            let statement = self.parse_statement();

            if statement != None {
                block.statements.push(statement.unwrap());
            }

            self.next_token();
        } 

        block
    }

    fn parse_if(&mut self) -> Option<Expression> {
        if !self.expect_peek(Variant::LeftParentheses){
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Variant::RightParentheses){
            return None;
        }

        if !self.expect_peek(Variant::LeftBrace){
            return None;
        }

        let consequence = self.parse_block();

        if self.peek_token_is(&Variant::Else) {
            self.next_token();

            if !self.expect_peek(Variant::LeftBrace){
                return None;
            }

            let alternative = self.parse_block();

            return Some(Expression::IfElse(Box::new(condition), consequence, alternative));
        }

        Some(Expression::If(Box::new(condition), consequence))
    }

    fn parse_function(&mut self) -> Option<Expression> {
        if !self.expect_peek(Variant::LeftParentheses){
            return None;
        }

        let parameters = self.parse_function_params();
        
        if !self.expect_peek(Variant::LeftBrace){
            return None;
        }

        let body = self.parse_block();

        Some(Expression::Function(parameters, body))
    }

    fn parse_function_params(&mut self) -> Vec<Expression> {
        let mut parameters = vec![];

        if self.peek_token_is(&Variant::RightParentheses){
            self.next_token();
            return parameters;
        }

        self.next_token();

        loop {
            let parameter = Expression::Identifier(self.current_token.value.clone().unwrap());
            parameters.push(parameter);

            if !self.peek_token_is(&Variant::Comma) {
                break;
            }

            self.next_token();
            self.next_token();
        }

        if !self.expect_peek(Variant::RightParentheses){
            return vec![];
        }

        parameters
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let mut left_expression = match &self.current_token.variant {
            Variant::Identifier => Expression::Identifier(self.current_token.value.clone().unwrap()),
            Variant::Integer => Expression::Integer(self.parse_integer().unwrap()),
            Variant::Bang | Variant::Minus => self.parse_prefix().unwrap(),
            Variant::Bool => Expression::Bool(self.parse_bool().unwrap()),
            Variant::LeftParentheses => self.parse_group().unwrap(),
            Variant::If => self.parse_if().unwrap(),
            Variant::Function => self.parse_function().unwrap(),
            _ => panic!("TODO: Implement more operators??: {:?}", self.current_token.variant),
        };


        while self.peek_token.variant != Variant::EndOfFile {
            if self.peek_token.variant != Variant::Semicolon && precedence < self.precedence(&self.peek_token.variant)
            {
                self.next_token();
                let t = self.current_token.clone();
                match t.variant {
                    Variant::Plus
                    | Variant::Minus
                    | Variant::Asterisk
                    | Variant::Slash
                    | Variant::GreaterThan
                    | Variant::LessThan
                    | Variant::Equals
                    | Variant::NotEqual => left_expression = self.parse_infix(left_expression),
                    _ => (),
                }
            }   
            return left_expression;
        }
    left_expression
}

    fn parse_prefix(&mut self) -> Option<Expression>{
        let prefix = match &self.current_token.variant {
            Variant::Bang => Some(Prefix::Bang),
            Variant::Minus => Some(Prefix::Minus),
            _ => None
        };

        if prefix != None {
            self.next_token();
            let expression = self.parse_expression(Precedence::Prefix);

            let prefix_expression = Expression::Prefix(prefix.unwrap(), Box::new(expression));

            return Some(prefix_expression);
        }
        
        return None;
    }

    fn parse_infix(&mut self, left_expression: Expression) -> Expression {

        let variant = match self.current_token.variant {
            Variant::Plus => Infix::Plus,
            Variant::Minus => Infix::Minus,
            Variant::Asterisk => Infix::Multiply,
            Variant::Slash => Infix::Divide,
            Variant::LessThan => Infix::LessThan,
            Variant::GreaterThan => Infix::GreaterThan,
            Variant::Equals => Infix::Equals,
            Variant::NotEqual => Infix::NotEqual,
            _ => panic!("invalid infix expression")
        };

        let precedence = self.precedence(&self.current_token.variant);
        self.next_token();

        let right_expression = self.parse_expression(precedence);

        Expression::Infix(Box::new(left_expression), variant, Box::new(right_expression))
    }
}

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

    let expected = [
        (Variant::Let, Expression::Identifier("dingus".to_string())),
        (Variant::Integer, Expression::Integer(5)),
        (Variant::Let, Expression::Identifier("y".to_string())),
        (Variant::Integer, Expression::Integer(10)),
        (Variant::Integer, Expression::Integer(838383)),
    ];

    assert_eq!(parser.errors.len(), 1);
    assert_eq!(program.statements.len(), 5);
    
    for i in 0..5 {
        let statement = &program.statements[i];
        
        let (x, y) = expected[i].clone();

        assert_eq!(statement.variant, x);
        assert_eq!(statement.expression.as_ref().unwrap(), &y);
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
        assert_eq!(s.variant, Variant::Return);
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
        assert_eq!(s.variant, Variant::Identifier);
        assert_eq!(s.expression.as_ref().unwrap(), &Expression::Identifier("foobar".to_string()));
    }
}

#[test]
fn integer_expressions() {
    let input = "
    5;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 1);
    
    for s in program.statements {
        assert_eq!(s.variant, Variant::Integer);
        assert_eq!(s.expression.as_ref().unwrap(), &Expression::Integer(5));
    }
}

#[test]
fn prefix_expressions() {
    let input = "
    !5;
    -15;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected = [
        (Variant::Bang, Expression::Prefix(Prefix::Bang, Box::new(Expression::Integer(5)))), 
        (Variant::Minus, Expression::Prefix(Prefix::Minus, Box::new(Expression::Integer(15))))
    ];

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 2);
    
    for i in 0..2 {
        let (x, y) = expected[i].clone();
        let statement = program.statements[i].clone();
        let expression = statement.expression.unwrap();

        assert_eq!(statement.variant, x);
        assert_eq!(expression, y);
    }
}

#[test]
fn infix_expressions() {
    let input = "
    5 + 5;
    5 - 5;
    5 * 5;
    5 / 5;
    5 > 5;
    5 < 5;
    5 == 5;
    5 != 5;
    true != false;
    true == true;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected = [
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::Plus, Box::new(Expression::Integer(5)))),
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::Minus, Box::new(Expression::Integer(5)))),
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::Multiply, Box::new(Expression::Integer(5)))),
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::Divide, Box::new(Expression::Integer(5)))),
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::GreaterThan, Box::new(Expression::Integer(5)))),
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::LessThan, Box::new(Expression::Integer(5)))),
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::Equals, Box::new(Expression::Integer(5)))),
        (Variant::Integer, Expression::Infix(Box::new(Expression::Integer(5)), Infix::NotEqual, Box::new(Expression::Integer(5)))),
        (Variant::Bool, Expression::Infix(Box::new(Expression::Bool(true)), Infix::NotEqual, Box::new(Expression::Bool(false)))),
        (Variant::Bool, Expression::Infix(Box::new(Expression::Bool(true)), Infix::Equals, Box::new(Expression::Bool(true)))),
    ];

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 10);
    
    for i in 0..10 {

        let (x, y) = expected[i].clone();
        let statement = program.statements[i].clone();
        let expression = statement.expression.unwrap();

        assert_eq!(statement.variant, x);
        assert_eq!(expression, y);
    }
}

#[test]
fn boolean_expressions() {
    let input = "
    true;
    false;
    let foobar = true;
    let barfoo = false;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected = [
        (Variant::Bool, Expression::Bool(true)),
        (Variant::Bool, Expression::Bool(false)),
        (Variant::Let, Expression::Identifier("foobar".to_string())),
        (Variant::Bool, Expression::Bool(true)),
        (Variant::Let, Expression::Identifier("barfoo".to_string())),
        (Variant::Bool, Expression::Bool(false))
    ];

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 6);
    
    for i in 0..6 {
        let s = &program.statements[i];
        let (x, y) = &expected[i];

        assert_eq!(&s.variant, x);
        assert_eq!(s.expression.as_ref().unwrap(), y);
    }
}

#[test]
fn if_expression() {
    let input = "if (x < y) { x }".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected = [
        Expression::If(
            Box::new(Expression::Infix(
                Box::new(Expression::Identifier("x".to_string())), 
                Infix::LessThan, 
                Box::new(Expression::Identifier("y".to_string())))), 
                BlockStatement::new_with_statements(
                    Variant::LeftBrace, 
                    vec![
                        Statement::new(
                            Variant::Identifier, 
                            Some(Expression::Identifier("x".to_string())))
                    ])
            )
    ];

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 1);
    
    let x = expected[0].clone();
    let statement = program.statements[0].clone();
    let expression = statement.expression.unwrap();

    assert_eq!(statement.variant, Variant::If);
    assert_eq!(expression, x);
}

#[test]
fn if_else_expression() {
    let input = "if (x < y) { x } else { y }".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected = [
        Expression::IfElse(
            Box::new(Expression::Infix(
                Box::new(Expression::Identifier("x".to_string())), 
                Infix::LessThan, 
                Box::new(Expression::Identifier("y".to_string())))), 
                BlockStatement::new_with_statements(
                    Variant::LeftBrace, 
                    vec![
                        Statement::new(
                            Variant::Identifier, 
                            Some(Expression::Identifier("x".to_string())))
                    ]),
                BlockStatement::new_with_statements(
                    Variant::LeftBrace, 
                    vec![
                        Statement::new(
                            Variant::Identifier, 
                            Some(Expression::Identifier("y".to_string())))
                    ]
                )
            )
    ];

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 1);
    
    let x = expected[0].clone();
    let statement = program.statements[0].clone();
    let expression = statement.expression.unwrap();

    assert_eq!(statement.variant, Variant::If);
    assert_eq!(expression, x);

}

#[test]
fn function_expression() {
    let input = "fn(x, y) { x + y; }".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected = 
        Expression::Function(
            vec![
                Expression::Identifier("x".to_string()),
                Expression::Identifier("y".to_string())
            ],
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![
                    Statement::new(
                        Variant::Identifier, 
                        Some(Expression::Infix(
                                Box::new(
                                    Expression::Identifier("x".to_string())
                                ), 
                            Infix::Plus, 
                            Box::new(
                                Expression::Identifier("y".to_string())
                                )
                            )
                        )
                    ),
                ]
            )
        );

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 1);
    
    let statement = program.statements[0].clone();
    let expression = statement.expression.unwrap();

    assert_eq!(statement.variant, Variant::Function);
    assert_eq!(expression, expected);
}