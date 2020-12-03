use crate::ast::*;
use crate::lexer::*;
use crate::object::*;
use crate::parser::*;
use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(PartialEq, Clone, Debug)]
pub struct Environment {
    store: HashMap<String, object::Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

fn NewEnvironment() -> Environment {
    let s: HashMap<String, object::Object> = HashMap::new();
    return Environment {
        store: s,
        outer: None,
    };
}

impl Environment {
    pub fn Get(&self, name: String) -> Option<object::Object> {
        match self.store.get(&name) {
            Some(value) => Some(value.clone()),
            _ => match self.outer {
                Some(ref outer) => outer.borrow_mut().Get(name),
                _ => None,
            },
        }
    }

    pub fn Set(&mut self, name: String, val: object::Object) -> Option<object::Object> {
        self.store.insert(name, val.clone());
        return Some(val);
    }
}

pub fn NewEnclosedEnvironment(outer: Rc<RefCell<Environment>>) -> Environment {
    let mut env = NewEnvironment();
    env.outer = Some(outer);
    return env;
}
