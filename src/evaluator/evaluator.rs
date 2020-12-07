use crate::ast::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub fn isError(obj: object::Object) -> bool {
    match obj {
        object::Object::ERROR_OBJ(_) => return true,
        _ => return false,
    }
}

pub fn nativeBoolToBooleanObject(input: bool) -> object::Object {
    if input {
        return object::Object::BOOLEAN_OBJ(true);
    }
    return object::Object::BOOLEAN_OBJ(false);
}

pub fn Eval(
    program: ast::Program,
    env: Rc<RefCell<environment::Environment>>,
) -> Option<object::Object> {
    let result = None;
    for stmt in program {
        match evalStatement(stmt, env) {
            Some(object::Object::RETURN_VALUE_OBJ(value)) => {
                return Some(object::Object::RETURN_VALUE_OBJ(value))
            }
            Some(object::Object::ERROR_OBJ(msg)) => return Some(object::Object::ERROR_OBJ(msg)),
            obj => result = obj,
        }
    }
    result
}

pub fn evalStatement(
    stmt: ast::Statement,
    env: Rc<RefCell<environment::Environment>>,
) -> Option<object::Object> {
    match stmt {
        ast::Statement::Let(ident, expr) => {
            let value = match evalExpression(expr, env) {
                Some(value) => value,
                _ => return None,
            };
            if isError(value) {
                Some(value)
            } else {
                let ast::Identifier { Value: name } = ident;
                env.borrow_mut().Set(name, value);
                None
            }
        }
    }
}

pub fn evalExpression(
    expr: ast::Expression,
    env: Rc<RefCell<environment::Environment>>,
) -> Option<object::Object> {
    match expr {
        ast::Expression::Identifier(ident) => Some(evalIdentifier(ident, env)),
        ast::Expression::Literal(literal) => Some(evalLiteral(literal, env)),
        ast::Expression::Prefix(prefix, right_expr) => {
            if let Some(right) = evalExpression(*right_expr, env) {
                Some(evalPrefixExpression(prefix, right, env))
            } else {
                None
            }
        }
        ast::Expression::Infix(infix, left_expr, right_expr) => {
            let left = match evalExpression(*left_expr, env){
                Some(value) => value,
                _ => return None,
            };
            if isError(left) {
                return Some(left);
            }
            let right = match evalExpression(*right_expr, env){
                Some(value) => value,
                _ => return None,
            };
            if isError(right) {
                return Some(right);
            }
            return Some(evalInfixExpression(infix, left, right, env));
        }
        ast::Expression::If {
            Condition: condition,
            Consequence: consequence,
            Alternative: alternative,
        } => Some(evalIfExpression(*condition, consequence, alternative, env)),
        ast::Expression::Call {
            Function: function,
            Arguments: arguments,
        } => Some(evalCallExpression(*function, arguments, env)),
        ast::Expression::Index {
            Left: left_expr,
            Index: index_expr,
        } => {
            let left = match evalExpression(*left_expr, env){
                Some(value) => value,
                _ => return None,
            };
            if isError(left) {
                return Some(left);
            }
            let index = match evalExpression(*index_expr, env){
                Some(value) => value,
                _ => return None,
            };
            if isError(index) {
                return Some(index);
            }
            return Some(evalIndexExpression(left, index, env));
        }
    }
}

pub fn evalIdentifier(
    ident: ast::Identifier,
    env: Rc<RefCell<environment::Environment>>,
) -> object::Object {
    let ast::Identifier { Value: name } = ident;
    match env.borrow_mut().Get(name.clone()) {
        Some(value) => value,
        _ => match object::Object::ERROR_OBJ(String::from(format!("identifier not found: {}", name))),
    }
}

pub fn evalPrefixExpression(prefix: ast::Prefix, right: object::Object, env: Rc<RefCell<environment::Environment>>,) -> object::Object {
    match prefix {
        ast::Prefix::BANG => return evalBangOperatorExpression(right),
        ast::Prefix::MINUS => return evalMinusPrefixOperatorExpression(right),
        _ => return object::Object::ERROR_OBJ(String::from(format!("unknown operator: {:?}{:?}", prefix, right))),
    }
}

pub fn evalBangOperatorExpression(right: object::Object) -> object::Object {
    match right {
        object::Object::BOOLEAN_OBJ(true) => object::Object::BOOLEAN_OBJ(false),
        object::Object::BOOLEAN_OBJ(false) => object::Object::BOOLEAN_OBJ(true),
        object::Object::NULL_OBJ => object::Object::BOOLEAN_OBJ(true),
        _ => return object::Object::BOOLEAN_OBJ(false),
    }
}

pub fn evalMinusPrefixOperatorExpression(right: object::Object) -> object::Object {
    if let object::Object::INTEGER_OBJ(value) = right {
        return object::Object::INTEGER_OBJ(-value);
    }else{
        return object::Object::ERROR_OBJ(String::from(format!("unknown operator: -{:?}", right)));
    }
}
