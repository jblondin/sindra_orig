use std;
use std::fmt;

use span::Span;
use lex;

#[derive(Debug)]
pub struct Error<'a> {
    pub origin: ErrorKind<'a>,
    pub span: Option<Span<'a>>,
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
            Some(span) => write!(f, "{} at line {}, column {}: {}{}",
                (self as &std::error::Error).description(), span.start.row, span.start.column,
                self.origin, span.context()),
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
pub struct MultiError<'a> {
    pub message: String,
    pub errors: Vec<Error<'a>>,
}
impl<'a> MultiError<'a> {
    pub fn new(message: String, errors: Vec<Error<'a>>) -> MultiError<'a> {
        MultiError {
            message: message,
            errors: errors,
        }
    }
}
impl<'a> fmt::Display for MultiError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: ", (self as &std::error::Error).description())?;
        for err in &self.errors {
            match err.span {
                Some(span) => write!(f, "\n{} at line {}, column {}: {}",
                    (err as &std::error::Error).description(), span.start.row, span.start.column,
                    err.origin)?,
                None       => write!(f, "\n{} (unknown position): {}",
                    (err as &std::error::Error).description(), err.origin)?,
            }
        }
        Ok(())
    }
}
impl<'a> std::error::Error for MultiError<'a> {
    fn description(&self) -> &str { &self.message }
    fn cause(&self) -> Option<&std::error::Error> { None }
}

#[derive(Debug)]
pub enum ErrorKind<'a> {
    Lex(String),
    LexToken(lex::errors::Error<'a>),
    Parse(String),
    Eval(String),
    Store(String)
}
impl<'a> fmt::Display for ErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::LexToken(ref e) => write!(f, "{}: {}",
                (self as &std::error::Error).description(), e),
            ErrorKind::Lex(ref s)
                | ErrorKind::Parse(ref s)
                | ErrorKind::Eval(ref s)
                | ErrorKind::Store(ref s)
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
            ErrorKind::Store(_)    => "variable error",
        }
    }
    fn cause(&self) -> Option<&std::error::Error> {
        match *self {
            ErrorKind::LexToken(ref e) => Some(e),
            _                          => None,
        }
    }
}

pub type MultiResult<'a, T> = std::result::Result<T, MultiError<'a>>;
pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
pub type KindResult<'a, T> = std::result::Result<T, ErrorKind<'a>>;
pub type ResOpt<'a, T> = Result<'a, Option<T>>;
