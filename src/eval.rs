use std::vec;

use crate::parser::Program;
use crate::ast::{Statement, Expression, Prefix, Infix, BlockStatement};
use crate::token::Variant;

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Bool(bool),
    Empty,
    ReturnValue(Box<Object>)
}

pub fn inspect(object: &Object) -> String {
    let result = match object {
        Object::Empty => "empty".to_string(),
        Object::Integer(x) => x.to_string(),
        Object::Bool(x) => x.to_string(),
        Object::ReturnValue(x) => inspect(x),
        _ => panic!("failed to inspect")
    };

    result
}

pub fn evaluate(program: Program) -> Vec<Object> {

    let mut objects: Vec<Object> = Vec::new();
    for statement in program.statements {
        objects.push(evaluate_statement(statement));
    }

    objects
}

fn evaluate_statement(statement: Statement) -> Object {
    let expr = match statement.expression {
        Some(expr) => evaluate_expression(expr),
        None => panic!("expected an expression but got none")
    };

    if statement.variant == Variant::Return{
        return Object::ReturnValue(Box::new(expr))
    }

    expr
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
        },

        Expression::If(condition, consequence) => evaluate_if_else_expression(*condition, consequence, None),
        Expression::IfElse(condition, consequence, alternative) => evaluate_if_else_expression(*condition, consequence, Some(alternative)),
        
        _ => panic!("{:?}", expression) 
    }
}

fn evaluate_block_statement(block: BlockStatement) -> Object {
    let mut result = Object::Empty;
    
    for s in block.statements{
        result = evaluate_statement(s);

        match result {
            Object::ReturnValue(_) => return result,
            _ => ()
        }
    }

    result
}

fn evaluate_if_else_expression(condition: Expression, consequence: BlockStatement, alternative: Option<BlockStatement>) -> Object {
    let condition = evaluate_expression(condition);

    if is_truthy(condition){
        return evaluate_block_statement(consequence);   
    }
    else if alternative != None {
        return evaluate_block_statement(alternative.unwrap())
    }
    
    Object::Empty    
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Bool(x) => x,
        Object::Empty => false,
        _ => true
    }
}

fn evaluate_infix_expression(left: Object, infix: Infix, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(x), Object::Integer(y)) => return evaluate_infix_integer_expression(x, infix, y),
        (Object::Bool(x), Object::Bool(y)) => return evaluate_infix_bool_expression(x, infix, y),
        _ => panic!("infix expression not implemented yet")
    }
}

fn evaluate_infix_bool_expression(left: bool, infix: Infix, right: bool) -> Object {
    match infix {
        Infix::Equals => Object::Bool(left == right),
        Infix::NotEqual => Object::Bool(left != right),
        _ => panic!("unexpected bool expression")
    }
}

fn evaluate_infix_integer_expression(left: i64, infix: Infix, right: i64) -> Object {
    match infix {
        Infix::Divide => Object::Integer(left / right),
        Infix::Minus => Object::Integer(left - right),
        Infix::Multiply => Object::Integer(left * right),
        Infix::Plus => Object::Integer(left + right),
        Infix::Equals => Object::Bool(left == right),
        Infix::LessThan => Object::Bool(left < right),
        Infix::GreaterThan => Object::Bool(left > right),
        Infix::NotEqual => Object::Bool(left != right),
        _ => panic!("unexpected integer expression")
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

    let evaluation = evaluate(input);

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

    let statements = vec![
        Statement::new(Variant::Bool, Some(Expression::Bool(false))),
        Statement::new(Variant::Bool, Some(Expression::Bool(true))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(1)), Infix::LessThan, Box::new(Expression::Integer(2))))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(1)), Infix::GreaterThan, Box::new(Expression::Integer(2))))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(1)), Infix::LessThan, Box::new(Expression::Integer(1))))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(1)), Infix::GreaterThan, Box::new(Expression::Integer(1))))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(1)), Infix::Equals, Box::new(Expression::Integer(1))))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(1)), Infix::NotEqual, Box::new(Expression::Integer(1))))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(2)), Infix::Equals, Box::new(Expression::Integer(1))))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(2)), Infix::NotEqual, Box::new(Expression::Integer(1))))),
        Statement::new(Variant::LeftParentheses, Some(Expression::Infix(Box::new(Expression::Infix(Box::new(Expression::Integer(1)), Infix::LessThan, Box::new(Expression::Integer(2)))), Infix::Equals, Box::new(Expression::Bool(true))))),
        Statement::new(Variant::LeftParentheses, Some(Expression::Infix(Box::new(Expression::Infix(Box::new(Expression::Integer(1)), Infix::LessThan, Box::new(Expression::Integer(2)))), Infix::Equals, Box::new(Expression::Bool(false))))),
        Statement::new(Variant::LeftParentheses, Some(Expression::Infix(Box::new(Expression::Infix(Box::new(Expression::Integer(1)), Infix::GreaterThan, Box::new(Expression::Integer(2)))), Infix::Equals, Box::new(Expression::Bool(true))))),
        Statement::new(Variant::LeftParentheses, Some(Expression::Infix(Box::new(Expression::Infix(Box::new(Expression::Integer(1)), Infix::GreaterThan, Box::new(Expression::Integer(2)))), Infix::Equals, Box::new(Expression::Bool(false)))))
    ];  

    let mut input = Program::new();
    input.statements = statements;

    let evaluation = evaluate(input);

    let expected = [ false, true, true, false, false, false, true, false, false, true, true, false, false, true ];

    for i in 0..14 {
        
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
    
    let evaluation = evaluate(input);

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
    
    let evaluation = evaluate(input);

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
    
    let evaluation = evaluate(input);

    let expected = [ 10, 32, 0, 100, 37 ];

    for i in 0..5 {
        
        let actual = match evaluation[i]{
            Object::Integer(x) => x,
            _ => panic!("unexpected input")
        };

        assert_eq!(actual, expected[i]);
    }
}

#[test]
fn can_evaluate_if_else_expressions() {
    let mut input = Program::new();

    let statements = vec!
    [
        Statement::new(Variant::If, Some(Expression::If(
            Box::new(Expression::Bool(true)), 
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(10)))
                ]
            )
        ))),

        Statement::new(Variant::If, Some(Expression::If(
            Box::new(Expression::Bool(false)), 
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(10)))
                ]
            )
        ))),

        Statement::new(Variant::If, Some(Expression::If(
            Box::new(Expression::Integer(1)), 
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(10)))
                ]
            )
        ))),

        Statement::new(Variant::If, Some(Expression::If(
            Box::new(Expression::Infix(
                Box::new(Expression::Integer(1)), 
                Infix::LessThan, 
                Box::new(Expression::Integer(2)))), 
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(10)))
                ]
            )
        )))

        ,

        Statement::new(Variant::If, Some(Expression::If(
            Box::new(Expression::Infix(
                Box::new(Expression::Integer(1)), 
                Infix::GreaterThan, 
                Box::new(Expression::Integer(2)))), 
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(10)))
                ]
            )
        ))),

        Statement::new(Variant::If, Some(Expression::IfElse(
            Box::new(Expression::Infix(
                Box::new(Expression::Integer(1)), 
                Infix::GreaterThan, 
                Box::new(Expression::Integer(2)))), 
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(10)))
                ]
            ),
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(20)))
                ]
            )
        ))),

        Statement::new(Variant::If, Some(Expression::IfElse(
            Box::new(Expression::Infix(
                Box::new(Expression::Integer(1)), 
                Infix::LessThan, 
                Box::new(Expression::Integer(2)))), 
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(10)))
                ]
            ),
            BlockStatement::new_with_statements(
                Variant::LeftBrace, 
                vec![Statement::new(
                    Variant::Integer, 
                    Some(Expression::Integer(20)))
                ]
            )
        )))
    ];

    input.statements = statements;
    
    let evaluation = evaluate(input);

    let expected = [ 
        Object::Integer(10),
        Object::Empty,
        Object::Integer(10),
        Object::Integer(10),
        Object::Empty,
        Object::Integer(20),
        Object::Integer(10)
    ];

    for i in 0..7 {
        
        assert_eq!(evaluation[i], expected[i]);
    }
}

#[test]
fn can_evaluate_return_statements() {
    let mut input = Program::new();

    let statements = vec!
    [
        Statement::new(Variant::Return, Some(Expression::Integer(10))), 
        Statement::new(Variant::If, 
                        Some(Expression::If(
                            Box::new(Expression::Infix(
                                Box::new(Expression::Integer(10)), 
                                Infix::GreaterThan, 
                                Box::new(Expression::Integer(1)))), 
                            BlockStatement::new_with_statements(
                                Variant::LeftBrace, 
                                vec![
                                    Statement::new(Variant::If, 
                                        Some(Expression::If(
                                                Box::new(Expression::Infix(
                                                    Box::new(Expression::Integer(10)), 
                                                    Infix::GreaterThan, 
                                                    Box::new(Expression::Integer(1)))), 
                                        BlockStatement::new_with_statements(
                                            Variant::LeftBrace, 
                                            vec![ 
                                                Statement::new(Variant::Return, Some(Expression::Integer(10)))
                                            ]
                                        )))),
                                        Statement::new(Variant::Return, Some(Expression::Integer(1)))
                                ]
                            ),
                            
                        ))
                    )
    ];

    input.statements = statements;
    
    let evaluation = evaluate(input);
    println!("{:?}", evaluation);
    let expected = [ 
        Object::ReturnValue(Box::new(Object::Integer(10))),
        Object::ReturnValue(Box::new(Object::Integer(10)))
    ];

    for i in 0..2 {
        
        assert_eq!(evaluation[i], expected[i]);
    }
}