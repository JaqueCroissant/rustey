use std::collections::HashMap;

use crate::parser::Program;
use crate::ast::{Statement, Expression, Prefix, Infix, BlockStatement};
use crate::token::Variant;
use crate::environment::Environment;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Function(Vec<Expression>, BlockStatement, Environment),
    Integer(i64),
    Bool(bool),
    Empty,
    ReturnValue(Box<Object>),
    Error(String)
}

pub fn inspect(object: &Object) -> String {
    let result = match object {
        Object::Empty => "empty".to_string(),
        Object::Integer(x) => x.to_string(),
        Object::Bool(x) => x.to_string(),
        Object::ReturnValue(x) => inspect(x),
        Object::Error(x) => x.to_string(),
        Object::Function(_, _, _) => panic!("not implemented")
    };

    result
}

pub fn evaluate(program: Program, environment: &mut Environment) -> Vec<Object> {

    let mut objects: Vec<Object> = Vec::new();
    for statement in program.statements {
        objects.push(evaluate_statement(statement, environment));
    }

    objects
}

fn evaluate_statement(statement: Statement, environment: &mut Environment) -> Object {

    let object = match statement.variant {
        Variant::Let => parse_let_statement(statement.expression, environment),
        _ => try_evaluate_expression(statement.expression, environment) 
    };

    if is_error(&object){
        return object;
    }

    if statement.variant == Variant::Return{
        return Object::ReturnValue(Box::new(object))
    }

    object
}

fn parse_let_statement(expression: Option<Expression>, environment: &mut Environment) -> Object {

    if expression == None {
        return create_error(format!("expected an expression but got none"));
    }

    let unwrapped_expr = expression.unwrap();

    let result = match unwrapped_expr {
        Expression::Identifier(name, value) => {
            if value == None {
                create_error(format!("expected a value for identifier {:?} but found none", name))
            }
            else{
                let evaluated_value = evaluate_expression(*value.unwrap(), environment);

                if is_error(&evaluated_value){
                    return evaluated_value;    
                }

                match environment.set(name.clone(), evaluated_value){
                    Err(_) => return create_error(format!("cannot declare {:?} twice", name)),
                    Ok(_) => () 
                }

                let v = environment.get(name).unwrap();
                return v.clone();
            }
        },
        _ => create_error(format!("expected an identifier but found {:?}", unwrapped_expr))
    };

    result
}

fn try_evaluate_expression(expression: Option<Expression>, environment: &mut Environment) -> Object {
    match expression {
        Some(expr) => return evaluate_expression(expr, environment),
        None => return create_error(format!("expected an expression but got none"))
    }
}

fn evaluate_expression(expression: Expression, environment: &mut Environment) -> Object {
    
    match expression {
        
        Expression::Integer(x) => return Object::Integer(x),
        
        Expression::Bool(x) => return Object::Bool(x),

        Expression::Identifier(name, value) => {
            match value {
                Some(expr) => evaluate_expression(*expr, environment),
                None => {
                    let env_value = environment.get(name.clone());

                    match env_value {
                        Some(x) => return x.clone(),
                        None => return create_error(format!("no value associated with identifier {:?}", name)) 
                    };
                } 
            }
        }
        
        Expression::Prefix(pfx, expr) => {
            let right = evaluate_expression(*expr, environment);

            if is_error(&right){
                return right;
            }

            return evaluate_prefix_expression(pfx, right)
        },

        Expression::Infix(left_expr, infix, right_expr) => {
            let left = evaluate_expression(*left_expr, environment);
            
            if is_error(&left){
                return left;
            }
            
            let right = evaluate_expression(*right_expr, environment);
            
            if is_error(&right){
                return right;
            }
            
            return evaluate_infix_expression(left, infix, right);
        },

        Expression::If(condition, consequence) => evaluate_if_else_expression(*condition, consequence, None, environment),
        
        Expression::IfElse(condition, consequence, alternative) => evaluate_if_else_expression(*condition, consequence, Some(alternative), environment),
        
        _ => create_error(format!("unexpected expression: {:?}", expression))
    }
}

fn is_error(object: &Object) -> bool {
    match object {
        Object::Error(_) => true,
        _ => false
    }
}

fn evaluate_block_statement(block: BlockStatement, environment: &mut Environment) -> Object {
    let mut result = Object::Empty;
    
    for s in block.statements{
        result = evaluate_statement(s, environment);

        match result {
            Object::ReturnValue(_) |
            Object::Error(_) => return result,
            _ => ()
        }
    }

    result
}

fn evaluate_if_else_expression(condition: Expression, consequence: BlockStatement, alternative: Option<BlockStatement>, environment: &mut Environment) -> Object {
    let condition = evaluate_expression(condition, environment);

    if is_truthy(condition){
        return evaluate_block_statement(consequence, environment);   
    }
    else if alternative != None {
        return evaluate_block_statement(alternative.unwrap(), environment)
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

fn create_error(message: String) -> Object{
    Object::Error(message.to_lowercase())
}

fn evaluate_infix_expression(left: Object, infix: Infix, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(x), Object::Integer(y)) => return evaluate_infix_integer_expression(x, infix, y),
        (Object::Bool(x), Object::Bool(y)) => return evaluate_infix_bool_expression(x, infix, y),

        (Object::Bool(x), Object::Integer(y)) => return create_error(format!("unknown operator: {:?} {:?} {:?}", x, infix, y)),
        (Object::Integer(x), Object::Bool(y)) => return create_error(format!("unknown operator: {:?} {:?} {:?}", x, infix, y)), 
        _ =>  return create_error(format!("invalid infix expression"))
    }
}

fn evaluate_infix_bool_expression(left: bool, infix: Infix, right: bool) -> Object {
    match infix {
        Infix::Equals => Object::Bool(left == right),
        Infix::NotEqual => Object::Bool(left != right),
        x => return create_error(format!("unknown operator: {:?} {:?} {:?}", left, x, right))
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
            Object::Bool(x) => create_error(format!("unknown operator: {:?} {:?}", prefix, x)),
            x => return create_error(format!("unknown operator: {:?} {:?}", prefix, x))
        }
    };

    result
}

#[cfg(test)]
#[test]
fn can_evaluate_integers() {
    let mut environment = Environment::new();
    let mut input = Program::new();
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(5))));
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(-48))));
    input.statements.push(Statement::new(Variant::Integer, Some(Expression::Integer(232323))));

    let evaluation = evaluate(input, &mut environment);

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

    let mut environment = Environment::new();
    let mut input = Program::new();
    input.statements = statements;

    let evaluation = evaluate(input, &mut environment);

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
    
    let mut environment = Environment::new();
    let evaluation = evaluate(input, &mut environment);

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
    
    let mut environment = Environment::new();
    let evaluation = evaluate(input, &mut environment);

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
    
    let mut environment = Environment::new();
    let evaluation = evaluate(input, &mut environment);

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
    
    let mut environment = Environment::new();
    let evaluation = evaluate(input, &mut environment);

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
    
    let mut environment = Environment::new();
    let evaluation = evaluate(input, &mut environment);
    
    let expected = [ 
        Object::ReturnValue(Box::new(Object::Integer(10))),
        Object::ReturnValue(Box::new(Object::Integer(10)))
    ];

    for i in 0..2 {
        assert_eq!(evaluation[i], expected[i]);
    }
}

#[test]
fn can_generate_error_messages() {
    let mut input = Program::new();

    let statements = vec!
    [
        Statement::new(Variant::Identifier, Some(Expression::Identifier("foobar".to_string(), None))),
        Statement::new(Variant::Integer, Some(Expression::Infix(Box::new(Expression::Integer(5)), Infix::Plus, Box::new(Expression::Bool(true))))),
        Statement::new(Variant::Minus, Some(Expression::Prefix(Prefix::Minus, Box::new(Expression::Bool(true))))),
        Statement::new(Variant::Bool, Some(Expression::Infix(Box::new(Expression::Bool(false)), Infix::Plus, Box::new(Expression::Bool(true))))),
        Statement::new(
            Variant::If, 
            Some(Expression::If(
                Box::new(Expression::Infix(
                    Box::new(Expression::Integer(10)), 
                    Infix::GreaterThan, 
                    Box::new(Expression::Integer(1)))), 
                BlockStatement::new_with_statements(
                    Variant::LeftBrace, 
                    vec![ 
                        Statement::new(
                            Variant::Bool, 
                            Some(Expression::Infix(
                                Box::new(Expression::Bool(false)), 
                                Infix::Plus, 
                                Box::new(Expression::Bool(true)))
                            )
                        )
                    ]
                ))
            )
        )
    ];

    input.statements = statements;
    
    let mut environment = Environment::new();
    let evaluation = evaluate(input, &mut environment);
    
    let expected = [
        Object::Error("no value associated with identifier \"foobar\"".to_string()), 
        Object::Error("unknown operator: 5 plus true".to_string()),
        Object::Error("unknown operator: minus true".to_string()),
        Object::Error("unknown operator: false plus true".to_string()),
        Object::Error("unknown operator: false plus true".to_string())
    ];

    for i in 0..5 {
        assert_eq!(evaluation[i], expected[i]);
    }
}

#[test]
fn can_evaluate_let_statements() {

    let programs = vec![
        Program::new_with_statements(vec![
            Statement::new(Variant::Let, Some(Expression::Identifier("a".to_string(), Some(Box::new(Expression::Integer(5)))))),
            Statement::new(Variant::Identifier, Some(Expression::Identifier("a".to_string(), None))),
        ]),
        Program::new_with_statements(vec![
            Statement::new(Variant::Let, Some(Expression::Identifier("a".to_string(), Some(Box::new(Expression::Infix(Box::new(Expression::Integer(5)), Infix::Multiply, Box::new(Expression::Integer(5)))))))),
            Statement::new(Variant::Identifier, Some(Expression::Identifier("a".to_string(), None))),
        ]),
        Program::new_with_statements(vec![
            Statement::new(Variant::Let, Some(Expression::Identifier("a".to_string(), Some(Box::new(Expression::Integer(5)))))),
            Statement::new(Variant::Let, Some(Expression::Identifier("b".to_string(), Some(Box::new(Expression::Identifier("a".to_string(), None)))))),
            Statement::new(Variant::Identifier, Some(Expression::Identifier("b".to_string(), None)))
        ]),
        Program::new_with_statements(vec![
            Statement::new(Variant::Let, Some(Expression::Identifier("a".to_string(), Some(Box::new(Expression::Integer(5)))))),
            Statement::new(Variant::Let, Some(Expression::Identifier("b".to_string(), Some(Box::new(Expression::Identifier("a".to_string(), None)))))),
            Statement::new(Variant::Let, Some(Expression::Identifier("c".to_string(), Some(Box::new(Expression::Infix(Box::new(Expression::Infix(Box::new(Expression::Identifier("a".to_string(), None)), Infix::Plus, Box::new(Expression::Identifier("b".to_string(), None)))), Infix::Plus, Box::new(Expression::Integer(5)))))))),
            Statement::new(Variant::Identifier, Some(Expression::Identifier("c".to_string(), None)))
        ])
    ];

    let expected = vec![ 5, 25, 5, 15 ];
    
    for (i, el) in programs.into_iter().enumerate() {
        
        let mut environment = Environment::new();
        
        let evaluation = evaluate(el, &mut environment);
        
        assert_eq!(*evaluation.last().unwrap(), Object::Integer(expected[i]));
    }
}