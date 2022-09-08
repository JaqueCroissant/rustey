use crate::parser::Program;
use crate::ast::{Statement, Expression, Prefix};
use crate::token::Variant;

pub enum Object {
    Integer(i64),
    Bool(bool),
    Empty
}

pub fn inspect(object: &Object) -> String {
    let result = match object {
        Object::Empty => "empty".to_string(),
        Object::Integer(x) => x.to_string(),
        Object::Bool(x) => x.to_string(),
        _ => panic!("failed to inspect")
    };

    result
}

pub fn evaluate_program(program: Program) -> Vec<Object> {

    let mut objects: Vec<Object> = Vec::new();
    for statement in program.statements {
        objects.push(evaluate_statement(statement));
    }

    objects
}

fn evaluate_statement(statement: Statement) -> Object {
    
    match statement.expression {
        Some(expr) => return evaluate_expression(expr),
        None => match statement.variant {
            _ => panic!("not implemented yet")
        } 
    }
}

fn evaluate_expression(expression: Expression) -> Object {
    
    match expression {
        Expression::Integer(x) => return Object::Integer(x),
        Expression::Bool(x) => return Object::Bool(x),
        Expression::Prefix(pfx, expr) => {
            let right = evaluate_expression(*expr);
            return evaluate_prefix_expression(pfx, right)
        },
        _ => panic!("not implemented yet") 
    }
}

fn evaluate_prefix_expression(prefix: Prefix, object: Object) -> Object {
    let result = match prefix {
        
        Prefix::Bang => {
            match object {
                Object::Bool(x) => Object::Bool(!x),
                Object::Empty => Object::Bool(true),
                _ => Object::Bool(false) 
            }
        },

        Prefix::Minus => match object {
            Object::Integer(x) => Object::Integer(-x),
            _ => Object::Empty
        }
    };

    result
}

#[cfg(test)]
#[test]
fn can_evaluate_integers() {
    let mut input = Program::new();
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(5))));
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(-48))));
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(232323))));

    let evaluation = evaluate_program(input);

    let expected = [ 5, -48, 232323 ];

    for i in 0..3 {
        
        let actual = match evaluation[i]{
            Object::Integer(x) => x,
            _ => panic!("unexpected input")
        };

        assert_eq!(actual, expected[i]);
    }
}

#[test]
fn can_evaluate_bools() {
    let mut input = Program::new();
    input.statements.push(Statement::new(Variant::Bool, Some(Expression::Bool(false))));
    input.statements.push(Statement::new(Variant::Bool, Some(Expression::Bool(true))));

    let evaluation = evaluate_program(input);

    let expected = [ false, true ];

    for i in 0..2 {
        
        let actual = match evaluation[i]{
            Object::Bool(x) => x,
            _ => panic!("unexpected input")
        };

        assert_eq!(actual, expected[i]);
    }
}

#[test]
fn can_evaluate_bang_prefix() {
    let mut input = Program::new();
    input.statements.push(Statement::new(Variant::Bang, Some(Expression::Prefix(Prefix::Bang, Box::new(Expression::Bool(true))))));
    input.statements.push(Statement::new(Variant::Bang, Some(Expression::Prefix(Prefix::Bang, Box::new(Expression::Bool(false))))));
    input.statements.push(Statement::new(Variant::Bang, Some(Expression::Prefix(Prefix::Bang, Box::new(Expression::Integer(5))))));
    input.statements.push(Statement::new(Variant::Bang, Some(Expression::Prefix(Prefix::Bang, Box::new(Expression::Prefix(Prefix::Bang, Box::new(Expression::Bool(true))))))));
    input.statements.push(Statement::new(Variant::Bang, Some(Expression::Prefix(Prefix::Bang, Box::new(Expression::Prefix(Prefix::Bang, Box::new(Expression::Bool(false))))))));
    input.statements.push(Statement::new(Variant::Bang, Some(Expression::Prefix(Prefix::Bang, Box::new(Expression::Prefix(Prefix::Bang, Box::new(Expression::Integer(5))))))));
    
    let evaluation = evaluate_program(input);

    let expected = [ false, true, false, true, false, true ];

    for i in 0..6 {
        
        let actual = match evaluation[i]{
            Object::Bool(x) => x,
            _ => panic!("unexpected input")
        };

        assert_eq!(actual, expected[i]);
    }
}

#[test]
fn can_evaluate_minus_prefix() {
    let mut input = Program::new();
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(5))));
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(10))));
    input.statements.push(Statement::new(Variant::Minus, Some(Expression::Prefix(Prefix::Minus, Box::new(Expression::Integer(5))))));
    input.statements.push(Statement::new(Variant::Minus, Some(Expression::Prefix(Prefix::Minus, Box::new(Expression::Integer(10))))));
    
    let evaluation = evaluate_program(input);

    let expected = [ 5, 10, -5, -10 ];

    for i in 0..4 {
        
        let actual = match evaluation[i]{
            Object::Integer(x) => x,
            _ => panic!("unexpected input")
        };

        assert_eq!(actual, expected[i]);
    }
}