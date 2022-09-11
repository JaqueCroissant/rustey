use crate::parser::Program;
use crate::ast::{Statement, Expression, Prefix, Infix};
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

        Expression::Infix(left_expr, infix, right_expr) => {
            let left = evaluate_expression(*left_expr);
            let right = evaluate_expression(*right_expr);
            return evaluate_infix_expression(left, infix, right);
        }
        _ => panic!("not implemented yet") 
    }
}

fn evaluate_infix_expression(left: Object, infix: Infix, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(x), Object::Integer(y)) => return evaluate_infix_integer_expression(x, infix, y),
        _ => panic!("infix expression not implemented yet")
    }
}

fn evaluate_infix_integer_expression(left: i64, infix: Infix, right: i64) -> Object {
    let value = match infix {
        Infix::Divide => left / right,
        Infix::Minus => left - right,
        Infix::Multiply => left * right,
        Infix::Plus => left + right,
        _ => panic!("unexpected integer expression")
    };

    Object::Integer(value)
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

#[test]
fn can_evaluate_infix_expressions() {
    let mut input = Program::new();

    let statements = vec!
    [
        // 5 + 5 + 5 + 5 - 10
        Statement::new(Variant::Integer, Some(
            Expression::Infix(
                Box::new(Expression::Integer(5)), 
                Infix::Plus,
                Box::new(Expression::Infix(
                    Box::new(Expression::Integer(5)), 
                    Infix::Plus, 
                    Box::new(Expression::Infix(
                        Box::new(Expression::Integer(5)), 
                            Infix::Plus,
                            Box::new(Expression::Infix(
                                Box::new(Expression::Integer(5)), 
                                Infix::Minus, 
                                Box::new(Expression::Integer(10))))))))))),
        
        // 2 * 2 * 2 * 2 * 2
        Statement::new(Variant::Integer, Some(
            Expression::Infix(
                Box::new(Expression::Integer(2)), 
                Infix::Multiply,
                Box::new(Expression::Infix(
                    Box::new(Expression::Integer(2)), 
                    Infix::Multiply, 
                    Box::new(Expression::Infix(
                        Box::new(Expression::Integer(2)), 
                            Infix::Multiply,
                            Box::new(Expression::Infix(
                                Box::new(Expression::Integer(2)), 
                                Infix::Multiply, 
                                Box::new(Expression::Integer(2))))))))))),
        
        // -50 + 100 + -50
        Statement::new(Variant::Minus, Some(
            Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Prefix(
                        Prefix::Minus, 
                        Box::new(Expression::Integer(50)))), 
                    Infix::Plus, 
                    Box::new(Expression::Integer(100)))), 
                Infix::Plus, 
                Box::new(Expression::Prefix(
                    Prefix::Minus, 
                    Box::new(Expression::Integer(50))))))),

        // 50 / 2 * 4
        Statement::new(Variant::Integer, Some(
            Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Integer(50)), 
                    Infix::Divide, 
                    Box::new(Expression::Integer(2)))), 
                    Infix::Multiply, 
                    Box::new(Expression::Integer(4))))),

        // 3 * (3 * 3) + 10
        Statement::new(Variant::Integer, Some(
            Expression::Infix(
                Box::new(Expression::Infix(
                    Box::new(Expression::Integer(3)), 
                    Infix::Multiply, 
                    Box::new(Expression::Infix(
                        Box::new(Expression::Integer(3)), 
                        Infix::Multiply, 
                        Box::new(Expression::Integer(3)))))), 
                Infix::Plus, 
                Box::new(Expression::Integer(10)))))
    ];

    input.statements = statements;
    
    let evaluation = evaluate_program(input);

    let expected = [ 10, 32, 0, 100, 37 ];

    for i in 0..5 {
        
        let actual = match evaluation[i]{
            Object::Integer(x) => x,
            _ => panic!("unexpected input")
        };

        assert_eq!(actual, expected[i]);
    }
}