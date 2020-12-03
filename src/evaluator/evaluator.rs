use crate::ast::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub fn Eval(program: ast::Program, env: Rc<RefCell<environment::Environment>>) -> object::Object {
    for stmt in program {
        match evalStatement(stmt) {}
    }
}

pub fn evalStatement(
    stmt: ast::Statement,
    env: Rc<RefCell<environment::Environment>>,
) -> Option<object::Object> {
    match stmt {
        ast::Statement::Let(ident, expr) => {
            let value = match evalExpression(expr) {
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
