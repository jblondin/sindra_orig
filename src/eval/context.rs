use std::hash::Hash;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Context<I, V>  where I: Eq + Hash {
    data: ContextData<I, V>,
}
impl<I, V> Context<I, V> where I: Eq + Hash {
    pub fn new() -> Context<I, V> {
        Context {
            data: ContextData::new(),
        }
    }
}

pub type ContextData<I, V> = HashMap<I, V>;
