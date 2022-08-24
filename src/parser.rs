use super::ast::{Expression, Statement, Prefix, Infix};
use super::lexer::Lexer;
use super::token::{Token,Variant};

enum Precedence {
    Lowest,
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
        let expression = self.parse_expression(Precedence::Lowest);
        
        if expression == None {
            return None;
        }

        let statement = Statement::new(self.current_token.variant.clone(), expression);

        if self.peek_token_is(&Variant::Semicolon){
            self.next_token();
        }

        Some(statement)
    }

    pub fn parse_generic(&mut self) -> Option<Expression> {

        let current = self.current_token;

        if current.value == None {
            return None;
        }

        let result = match current.variant {
            Variant::Identifier => Expression::Identifier(current.value.unwrap()),
            Variant::Integer => Expression::Integer(current.value.unwrap()),
            x => {
                let message = format!("expected identifier or integer but got {:?}", x);
                self.errors.push(message);
                return None;
            }
        };

        Some(result)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression>{
        let prefix_expr = self.parse_prefix();
        let expression = self.parse_generic();
                
       
        expression
        
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

            if expression == None {
                return None;
            }

            let prefix_expression = Expression::Prefix(prefix.unwrap(), Box::new(expression.unwrap()));

            return Some(prefix_expression);
        }
        else{
            return None;
        }
    }

    fn parse_infix(&mut self, left_expression: Expression) -> Expression {
        let current_token = self.current_token;

        let variant = match current_token.variant {
            Variant::Plus => Infix::Plus,
            Variant::Minus => Infix::Minus,
            Variant::Asterisk => Infix::Multiply,
            Variant::Slash => Infix::Divide,
            Variant::LessThan => Infix::LessThan,
            Variant::GreaterThan => Infix::GreaterThan,
            Variant::Equals => Infix::Equals,
            Variant::NotEqual => Infix::NotEqual,
            _ => panic!("the given value is an illegal infix character")
        };


        let precedence = self.current_precedence();
        self.next_token();

        let right_expression

        let expression = Expression::Infix(variant, current_token.value.unwrap());


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
        (Variant::Integer, Expression::Integer("5".to_string())),
        (Variant::Let, Expression::Identifier("y".to_string())),
        (Variant::Integer, Expression::Integer("10".to_string())),
        (Variant::Integer, Expression::Integer("838383".to_string())),
    ];

    assert_eq!(parser.errors.len(), 1);
    assert_eq!(program.statements.len(), 5);
    
    for i in 0..5 {
        let statement = &program.statements[i];
        
        let (x, y) = expected[i].clone();

        assert_eq!(statement.token_variant, x);
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
        assert_eq!(s.token_variant, Variant::Integer);
        assert_eq!(s.expression.as_ref().unwrap(), &Expression::Integer("5".to_string()));
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
        Expression::Prefix(Prefix::Bang, Box::new(Expression::Integer("5".to_string()))), 
        Expression::Prefix(Prefix::Minus, Box::new(Expression::Integer("15".to_string())))];

    println!("{:?}", program);

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 2);
    
    for i in 0..2 {

        let x = expected[i].clone();
        let statement = program.statements[i].clone();
        let expression = statement.expression.unwrap();

        assert_eq!(statement.token_variant, Variant::Integer);
        assert_eq!(expression, x);
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
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected = [
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::Plus, Box::new(Expression::Integer("5".to_string()))),
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::Minus, Box::new(Expression::Integer("5".to_string()))),
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::Multiply, Box::new(Expression::Integer("5".to_string()))),
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::Divide, Box::new(Expression::Integer("5".to_string()))),
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::GreaterThan, Box::new(Expression::Integer("5".to_string()))),
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::LessThan, Box::new(Expression::Integer("5".to_string()))),
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::Equals, Box::new(Expression::Integer("5".to_string()))),
        Expression::Infix(Box::new(Expression::Integer("5".to_string())), Infix::NotEqual, Box::new(Expression::Integer("5".to_string())))
    ];

    println!("{:?}", program);

    assert_eq!(parser.errors.len(), 0);
    assert_eq!(program.statements.len(), 2);
    
    for i in 0..2 {

        let x = expected[i].clone();
        let statement = program.statements[i].clone();
        let expression = statement.expression.unwrap();

        assert_eq!(statement.token_variant, Variant::Integer);
        assert_eq!(expression, x);
    }
}