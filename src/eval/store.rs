use std::hash::Hash;
use std::collections::HashMap;
use errors::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Store<I, V>  where I: Eq + Hash {
    stack: Vec<Frame<I, V>>,
}
impl<I, V> Store<I, V> where I: Eq + Hash {
    pub fn new() -> Store<I, V> {
        Store {
            stack: {
                let mut stack = vec![];
                // add the global frame
                stack.push(Frame::new());
                stack
            }
        }
    }
    pub fn set(&mut self, ident: I, value: V) {
        self.curr_frame().insert(ident, value);
    }
    pub fn get(&mut self, ident: I) -> Option<&V> {
        for i in (0..self.stack.len()).rev() {
            if let Some(ref value) = self.stack[i].get(&ident) {
                return Some(&value);
            }
        }
        None
    }
    pub fn push(&mut self) {
        self.stack.push(Frame::new());
    }
    pub fn pop(&mut self) -> ::std::result::Result<Frame<I, V>, ErrorKind> {
        debug_assert!(!self.stack.is_empty());
        if self.stack.len() == 1 {
            return Err(ErrorKind::Eval("attempt to remove global data frame".to_string()));;
        }
        Ok(self.stack.pop().unwrap())
    }
    fn curr_frame(&mut self) -> &mut Frame<I, V> {
        debug_assert!(!self.stack.is_empty());
        self.stack.last_mut().unwrap()
    }
}

pub type Frame<I, V> = HashMap<I, V>;
