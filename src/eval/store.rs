use std::hash::Hash;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Store<I, V>  where I: Eq + Hash {
    data: StoreData<I, V>,
}
impl<I, V> Store<I, V> where I: Eq + Hash {
    pub fn new() -> Store<I, V> {
        Store {
            data: StoreData::new(),
        }
    }
}

pub type StoreData<I, V> = HashMap<I, V>;
