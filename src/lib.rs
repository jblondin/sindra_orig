#![feature(plugin)]
#![plugin(sindra_plugin)]

#[macro_use] extern crate lazy_static;
extern crate regex;

pub mod errors;
pub mod span;
pub mod pprint;

#[macro_use] pub mod lex;
#[macro_use] pub mod parse;
#[macro_use] pub mod eval;
#[macro_use] pub mod interp;

#[macro_use] pub mod test_utils;
