use std::cell::RefCell;
use std::rc::Rc;
use std::hash::Hash;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Store<I, V>  where I: Eq + Hash, V: Clone {
    frame: Rc<RefCell<Frame<I, V>>>,
    parent: Option<Box<Store<I, V>>>
}
// clone implementation necessary since derive(Clone) expects a clone-able I and V, but we really
// just need to clone the frame Rc (which doesn't require cloning the underlying hashmap)
impl<I, V> Clone for Store<I, V> where I: Eq + Hash, V: Clone {
    fn clone(&self) -> Store<I, V> {
        Store {
            frame: self.frame.clone(),
            parent: self.parent.clone(),
        }
    }
}
impl<I, V> Store<I, V> where I: Eq + Hash, V: Clone {
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

    pub fn set(&self, ident: I, value: V) {
        self.frame.borrow_mut().insert(ident, value);
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
