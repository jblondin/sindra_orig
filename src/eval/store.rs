use std::fmt::Display;
use std::cell::RefCell;
use std::rc::Rc;
use std::hash::Hash;
use std::collections::HashMap;

use errors::*;

#[derive(Debug, PartialEq)]
pub struct Store<I, V>  where I: Eq + Hash + Display, V: Clone {
    frame: Rc<RefCell<Frame<I, V>>>,
    parent: Option<Box<Store<I, V>>>
}
// clone implementation necessary since derive(Clone) expects a clone-able I and V, but we really
// just need to clone the frame Rc (which doesn't require cloning the underlying hashmap)
impl<I, V> Clone for Store<I, V> where I: Eq + Hash + Display, V: Clone {
    fn clone(&self) -> Store<I, V> {
        Store {
            frame: self.frame.clone(),
            parent: self.parent.clone(),
        }
    }
}
impl<I, V> Store<I, V> where I: Eq + Hash + Display, V: Clone {
    pub fn new() -> Store<I, V> {
        Store {
            frame: Rc::new(RefCell::new(Frame::new())),
            parent: None,
        }
    }
    pub fn new_child(parent: &Store<I, V>) -> Store<I, V> {
        Store {
            frame: Rc::new(RefCell::new(Frame::new())),
            // since store only contains an Rc and a pointer to another store, this clone is cheap
            parent: Some(Box::new(parent.clone()))
        }
    }
    pub fn push(&self) -> Store<I, V> {
        Store::new_child(self)
    }
    pub fn pop(&self) -> Option<Box<Store<I, V>>> {
        match self.parent {
            Some(ref boxed) => Some((*boxed).clone()),
            None => None
        }
    }

    // returns previous value if this identifier has been previous declared in this scope
    pub fn declare(&self, ident: I, value: V) -> Option<V> {
        self.frame.borrow_mut().insert(ident, value)
    }
    pub fn mutate<'a>(&self, ident: I, new_value: V) -> KindResult<'a, V> {
        if self.frame.borrow().contains_key(&ident) {
            // key exists, so unwrap safe
            Ok(self.frame.borrow_mut().insert(ident, new_value).unwrap())
        } else {
            match self.parent {
                Some(ref parent) => (*parent).mutate(ident, new_value),
                None => Err(ErrorKind::Store(format!("variable not found: {}", ident)))
            }
        }
    }
    pub fn get(&self, ident: &I) -> Option<V> {
        match self.frame.borrow().get(ident) {
            Some(&ref value) => Some(value.clone()),
            None => {
                match self.parent {
                    Some(ref parent) => (*parent).get(ident),
                    None => None
                }
            }
        }
    }
}

pub type Frame<I, V> = HashMap<I, V>;
