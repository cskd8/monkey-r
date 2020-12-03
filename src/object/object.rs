use crate::ast::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub type BuiltinFunc = fn(Vec<Object>) -> Object;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    INTEGER_OBJ(i64),
    BOOLEAN_OBJ(bool),
    NULL_OBJ,
    RETURN_VALUE_OBJ(Box<Object>),
    ERROR_OBJ(String),
    FUNCTION_OBJ(
        Vec<ast::Identifier>,
        ast::BlockStatement,
        Rc<RefCell<environment::Environment>>,
    ),
    STRING_OBJ(String),
    BUILTIN_OBJ(i32, BuiltinFunc),
    ARRAY_OBJ(Vec<Object>),
    HASH_OBJ(HashMap<Object, Object>),
}

impl Eq for Object {}
impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::INTEGER_OBJ(ref i) => i.hash(state),
            Object::BOOLEAN_OBJ(ref b) => b.hash(state),
            Object::STRING_OBJ(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
