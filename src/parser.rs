use std::mem;
use super::lexer::Lexer;
use super::token::Token;


struct Program {
    pub statements: Vec<Statement>
}

impl Program {
    fn new() -> Program {
        return Program { statements: vec![] }
    }
}

struct Identifier{
    pub token: Token,
    pub value: String    
}

impl Identifier {
    pub fn new(value: String) -> Identifier{
        let token = Token::Identifier(value.to_string());
        Identifier { token, value }
    }
}

struct Expression{}

struct Statement{
    pub token: Token,
    pub name: Identifier,
    pub value: Expression
}

impl Statement {
    pub fn new(token: Token, identifier: Identifier) -> Statement {
        let statement = Statement { 
            token,
            name: identifier,
            value: Expression{}
          };

        statement
    }
}

pub struct Parser{
    lexer: Lexer,
    current_token: Token,
    peek_token: Token
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        
        let mut parser = Parser { 
            lexer, 
            current_token: Token::Illegal, 
            peek_token: Token::Illegal 
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let result = match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => None
        };

        result
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        
        if !self.expect_peek(&Token::Identifier("".to_string())) {
            return None;
        };

        // TODO: fuckin fix me
        let value = match &self.current_token { 
            Token::Identifier(x) => x,
            _ => panic!()
        };

        if !self.expect_peek(&Token::Assign) {
          return None;
        };

        let identifier = Identifier::new(value.to_string());
        let statement = Statement::new(Token::Let, identifier);

        while self.current_token == Token::Semicolon {
            self.next_token();
        }

        Some(statement)
    }

    fn expect_peek(&mut self, token: &Token) -> bool{
        if self.peek_token_is(token){
            self.next_token();
            return true;
        }
        false
    }

    fn current_token_is(&self, token: &Token) -> bool{
        self.variant_eq::<Token>(&self.current_token, token)
    }

    fn peek_token_is(&self, token: &Token) -> bool{
        self.variant_eq::<Token>(&self.peek_token, token)
    }

    fn variant_eq<T>(&self, a: &T, b: &T) -> bool {
        std::mem::discriminant(a) == std::mem::discriminant(b)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token != Token::EndOfFile {
            let statement = self.parse_statement();

            match statement {
                Some(x) => program.statements.push(x),
                None => (),
            }
            
            self.next_token();
        }

        program
    }
}

#[cfg(test)]
#[test]
fn let_statements() {
    let input = "
    let x = 5;
    let y = 10;
    let foobar = 838383;
    ".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    let expected_identifiers = [
        "x".to_string(), 
        "y".to_string(), 
        "foobar".to_string()
    ];

    assert_eq!(program.statements.len(), 3);

    for (i, el) in expected_identifiers.iter().enumerate() {
        let statement = &program.statements[i];

        assert_eq!(statement.token, Token::Let);
        assert_eq!(statement.name.value, expected_identifiers[i]);
    }
}