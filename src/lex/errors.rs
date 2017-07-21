use std::{self, fmt};

#[derive(Debug)]
pub struct Error<'a> {
    match_str: &'a str,
    kind: ErrorKind,
}
impl<'a> Error<'a> {
    pub fn new(match_str: &'a str, kind: ErrorKind) -> Error<'a> {
        Error {
            match_str: match_str,
            kind: kind,
        }
    }
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}
impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "in text '{}': {}", self.match_str, self.kind)
    }
}
impl<'a> std::error::Error for Error<'a> {
    fn description(&self) -> &str {
        "token lex error"
    }
    fn cause(&self) -> Option<&std::error::Error> {
        Some(&self.kind)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    ConvertStr(String),
    ConvertIntError(std::num::ParseIntError),
    ConvertFloatError(std::num::ParseFloatError),
    EscapeStr(String),
    EscapeUtf8Error(std::str::Utf8Error),
    EscapeIntError(std::num::ParseIntError),
}
impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::ConvertStr(ref s) | ErrorKind::EscapeStr(ref s) =>
                write!(f, "{}: {}", (self as &::std::error::Error).description(), s),
            ErrorKind::ConvertIntError(ref e)
                => write!(f, "{}: {}", (self as &::std::error::Error).description(), e),
            ErrorKind::ConvertFloatError(ref e)
                => write!(f, "{}: {}", (self as &::std::error::Error).description(), e),
            ErrorKind::EscapeUtf8Error(ref e)
                => write!(f, "{}: {}", (self as &::std::error::Error).description(), e),
            ErrorKind::EscapeIntError(ref e)
                => write!(f, "{}: {}", (self as &::std::error::Error).description(), e),
        }
    }
}
impl std::error::Error for ErrorKind {
    fn description(&self) -> &str {
        match *self {
            ErrorKind::ConvertStr(_)
                | ErrorKind::ConvertIntError(_)
                | ErrorKind::ConvertFloatError(_)
                => "conversion error",
            ErrorKind::EscapeStr(_)
                | ErrorKind::EscapeUtf8Error(_)
                | ErrorKind::EscapeIntError(_)
                => "escape error",
        }
    }
    fn cause(&self) -> Option<&std::error::Error> {
        match *self {
            ErrorKind::EscapeStr(_) | ErrorKind::ConvertStr(_)  => None,
            ErrorKind::ConvertIntError(ref e)                   => Some(e),
            ErrorKind::ConvertFloatError(ref e)                 => Some(e),
            ErrorKind::EscapeUtf8Error(ref e)                   => Some(e),
            ErrorKind::EscapeIntError(ref e)                    => Some(e),
        }
    }
}

pub mod rule {
    pub type Result<T> = ::std::result::Result<T, super::ErrorKind>;
}
pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

// #[derive(Debug, Clone, PartialEq)]
// pub struct DefaultCustomError(String);

// impl fmt::Display for DefaultCustomError {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{}", self.description())
//     }
// }
// impl Error for DefaultCustomError {
//     fn description(&self) -> &str { self.0.as_str() }
//     fn cause(&self) -> Option<&std::error::Error> { None }
// }

// pub type Result<T, E=DefaultCustomError> = std::result::Result<T, ErrorKind<E>>;

