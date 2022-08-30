use crate::parser::Program;
use crate::ast::{Statement, Expression};
use crate::token::Variant;

pub enum Object {
    Integer(i32),
    Bool(bool),
    Null
}

pub fn evaluate_program(program: Program) -> Vec<Object> {

    let mut objects: Vec<Object> = Vec::new();
    for statement in program.statements {
        objects.push(evaluate(statement));
    }

    objects
}

fn evaluate(statement: Statement) -> Object {
    
    match statement.variant {
        Variant::Integer => {
            let value = match statement.expression.unwrap() {
                Expression::Integer(x) => x,
                _ => panic!("unexpected expression")
            };
            return Object::Integer(value);
        }
        _ => panic!("not implemented yet")
    }
}

#[cfg(test)]
#[test]
fn can_evaluate_integers() {
    use crate::ast::Statement;


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