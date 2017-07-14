use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Offset {
    // number of bytes consumed
    nbytes: usize,

    // number of columns consumed
    // if no newlines were encountered, this should be the number of characters in the input
    // if a newline has been consumed, this is the number of characters since the last newline
    columns: usize,

    // number of newlines encountered
    lines: usize,
}
impl Offset {
    pub fn from_str(input: &str) -> Offset {
        Offset {
            nbytes: input.len(),
            columns: match input.rfind('\n') {
                Some(byte_index) => input[byte_index..].chars().count(),
                None => input.chars().count(),
            },
            lines: input.chars().filter(|&c| c == '\n').count(),
        }
    }
    pub fn one() -> Offset {
        Offset {
            nbytes: 1,
            columns: 1,
            lines: 0,
        }
    }
    pub fn nbytes(&self) -> usize { self.nbytes }
    fn has_newline(&self) -> bool { self.lines > 0 }
    pub fn apply_to(&self, pos: &mut Position) {
        pos.byte += self.nbytes;
        if self.has_newline() {
            pos.column = self.columns;
            pos.row += self.lines;
        } else {
            pos.column += self.columns;
        }
    }
}


// the position in a file
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    // current byte, 0-indexed
    pub byte: usize,

    // row position. 1-indexed
    pub row: usize,

    // column position, measured in chars.  1-indexed
    pub column: usize,

}
impl Position {
    pub fn start() -> Position {
        Position {
            byte: 0,
            row: 1,
            column: 1,
        }
    }
    pub fn new(byte: usize, row: usize, column: usize) -> Position {
        Position {
            byte: byte,
            row: row,
            column: column,
        }
    }

    pub fn offset(&mut self, offset: &Offset) {
        offset.apply_to(self);
    }
}

/// Lexer details about a token.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span<'a> {
    input: &'a str,
    pub pos: Position,
}

impl<'a> Span<'a> {
    pub fn new(input: &'a str, pos: Position) -> Span<'a> {
        Span {
            pos: pos,
            input: input,
        }
    }
    pub fn start(input: &'a str) -> Span<'a> {
        Span::new(input, Position::start())
    }
    pub fn as_slice(&self) -> &'a str {
        self.input
    }
    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan<'a, T> {
    pub token: T,

    pub span: Span<'a>,
}
impl<'a, T> TokenSpan<'a, T> where T: Debug + Clone + PartialEq {
    pub fn new(token: T, span: Span<'a>) -> TokenSpan<'a, T> {
        TokenSpan {
            token: token,
            span: span,
        }
    }
    pub fn is_token(&self, token: &T) -> bool {
        &self.token == token
    }
    pub fn matches_token(&self, other: &TokenSpan<'a, T>) -> bool {
        &self.token == &other.token
    }
}
