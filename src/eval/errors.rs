use std;
use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub enum ErrorKind {
    Eval(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::Eval(ref s)              => write!(f, "{}: {}", self.description(), s),
        }
    }
}

impl Error for ErrorKind {
    fn description(&self) -> &str { "evaluator error" }
    fn cause(&self) -> Option<&Error> { None }
}

pub type Result<T> = std::result::Result<T, ErrorKind>;
