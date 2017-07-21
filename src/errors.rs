use std;
use std::fmt;

use span::Span;
use lex;

#[derive(Debug)]
pub struct Error<'a> {
    origin: ErrorKind<'a>,
    span: Option<Span<'a>>,
}
impl<'a> Error<'a> {
    pub fn spanned(origin: ErrorKind<'a>, span: Span<'a>) -> Error<'a> {
        Error {
            origin: origin,
            span: Some(span),
        }
    }
    pub fn nospan(origin: ErrorKind<'a>) -> Error<'a> {
        Error {
            origin: origin,
            span: None,
        }
    }
}
impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.span {
            Some(span) => write!(f, "{} at line {}, column {}: {}",
                (self as &std::error::Error).description(), span.pos.row, span.pos.column,
                self.origin),
            None       => write!(f, "{} (unknown position): {}",
                (self as &std::error::Error).description(), self.origin),
        }
    }
}
impl<'a> std::error::Error for Error<'a> {
    fn description(&self) -> &str { "error" }
    fn cause(&self) -> Option<&std::error::Error> { Some(&self.origin) }
}

#[derive(Debug)]
pub enum ErrorKind<'a> {
    Lex(String),
    LexToken(lex::errors::Error<'a>),
    Parse(String),
    Eval(String),
}
impl<'a> fmt::Display for ErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::LexToken(ref e) => write!(f, "{}: {}",
                (self as &std::error::Error).description(), e),
            ErrorKind::Lex(ref s)
                | ErrorKind::Parse(ref s)
                | ErrorKind::Eval(ref s)
            => write!(f, "{}: {}", (self as &std::error::Error).description(), s),
        }
    }
}
impl<'a> std::error::Error for ErrorKind<'a> {
    fn description(&self) -> &str {
        match *self {
            ErrorKind::Lex(_)      => "lex error",
            ErrorKind::LexToken(_) => "token lex error",
            ErrorKind::Parse(_)    => "parse error",
            ErrorKind::Eval(_)     => "evalulation error",
        }
    }
    fn cause(&self) -> Option<&std::error::Error> {
        match *self {
            ErrorKind::LexToken(ref e) => Some(e),
            _                          => None,
        }
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
pub type ResOpt<'a, T> = std::result::Result<Option<T>, Error<'a>>;
