//TODO:
// * make group_tokens optional
// * remove reliance on precedence_type
// * make statements optional
#[macro_export]
macro_rules! parser {
    (
        token_type: $token_type:ty,
        group_tokens: ($group_token_start:path, $group_token_end:path),
        statements: [
            $($statement_name:tt($($statement_args_tt:tt)*)
                := { $($statement_tt:tt)* })*
        ],
        literals: [
            $($literal_token:path => $literal_name:ident<$literal_type:ty>,)*
        ],
        precedence_type: $precedence_type:ty,
        prefix<$prefix_precedence:path>: [
            $($prefix_token:path => $prefix_name:ident,)*
        ],
        infix: [
            $($infix_token:path => $infix_name:ident => $infix_precedence:expr,)*
        ]
    ) => (

use $crate::errors as perr;
use $crate::span as pspan;

use $crate::parse::precedence::Lowest;
// use $crate::parse::from_token::FromToken;

pub trait Precedence {
    type OpPrecedence: Lowest;

    fn precedence(&self) -> Option<Self::OpPrecedence>;
}

pub trait FromToken<T>: Sized {
    fn from_token(tok: &T) -> Option<Self>;
}

type SpannedToken<'a> = pspan::Spanned<'a, $token_type>;

pub type Program<'a> = Block<'a>;
pub type Block<'a> = Vec<SpannedStatement<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    $(
        $statement_name(statement_args!($($statement_args_tt)*))
    ),*
}
type SpannedStatement<'a> = pspan::Spanned<'a, Statement<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    $($literal_name($literal_type),)*
}
impl FromToken<$token_type> for Literal {
    fn from_token(tok: &$token_type) -> Option<Literal> {
        match *tok {
            $($literal_token(value) => Some(Literal::$literal_name(value)),)*
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal),
    Infix {
        op: InfixOp,
        left: Box<SpannedExpr<'a>>,
        right: Box<SpannedExpr<'a>>
    },
    Prefix {
        op: PrefixOp,
        right: Box<SpannedExpr<'a>>,
    },
}
type SpannedExpr<'a> = pspan::Spanned<'a, Expression<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    $($prefix_name,)*
}
impl FromToken<$token_type> for PrefixOp {
    fn from_token(tok: &$token_type) -> Option<PrefixOp> {
        match *tok {
            $($prefix_token => Some(PrefixOp::$prefix_name),)*
            _ => None
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum InfixOp {
    $($infix_name,)*
}
impl Precedence for InfixOp {
    type OpPrecedence = $precedence_type;

    fn precedence(&self) -> Option<Self::OpPrecedence> {
        match *self {
            $(InfixOp::$infix_name => Some($infix_precedence),)*
        }
    }
}
impl FromToken<$token_type> for InfixOp {
    fn from_token(tok: &$token_type) -> Option<InfixOp> {
        match *tok {
            $($infix_token => Some(InfixOp::$infix_name),)*
            _ => None
        }
    }
}

impl<'a> Precedence for SpannedToken<'a> {
    type OpPrecedence = $precedence_type;

    fn precedence(&self) -> Option<Self::OpPrecedence> {
        InfixOp::from_token(&self.item).and_then(|io| io.precedence())
    }
}

type ParserState<'a> = ::std::iter::Peekable<::std::slice::Iter<'a, SpannedToken<'a>>>;

pub fn parse<'a>(tokens: &'a Vec<SpannedToken<'a>>) -> perr::Result<'a, Program<'a>> {
    let mut state = tokens.iter().peekable();
    let mut program: Program = Program::new();
    while state.peek().is_some() {
        program.push(parse_statement(&mut state)?);
    }
    Ok(program)
}

fn parse_statement<'a>(state: &mut ParserState<'a>) -> perr::Result<'a, SpannedStatement<'a>> {
    debug_assert!(state.peek().is_some());
    $(
        match _parse_statement::$statement_name(state)? {
            Some(s) => {
                return Ok(s);
            },
            None => {
                // continue on to try next statement
            }
        }
    )*
    Err(perr::Error::nospan(perr::ErrorKind::Parse("no statement found".to_string())))
}

fn parse_expression<'a>(
    state: &mut ParserState<'a>,
    precedence: $precedence_type,
) -> perr::ResOpt<'a, SpannedExpr<'a>> {
    debug_assert!(state.peek().is_some());
    let prefix_opt = parse_prefix(state)?;
    if prefix_opt.is_none() {
        return Ok(None);
    }

    cond!(
        if empty($($infix_token,)*) {
            Ok(Some(prefix_opt.unwrap()))
        } else {
            let mut left_expr = prefix_opt.unwrap();
            while precedence < peek_precedence(state) {
                let infix_expr = match parse_infix(state)? {
                    Some((op, right_expr)) => {
                        let left_span = left_expr.span;
                        pspan::Spanned::new(
                            Expression::Infix {
                                op: op,
                                left: Box::new(left_expr),
                                right: Box::new(right_expr),
                            },
                            left_span
                        )
                    },
                    None => {
                        return Ok(Some(left_expr));
                    }
                };
                left_expr = infix_expr;
            }
            Ok(Some(left_expr))
        }
    )
}

fn peek_precedence<'a>(state: &mut ParserState<'a>) -> $precedence_type {
    // get the precedence of the next token (lowest if no token exists)
    if let Some(&&ref peek_tok) = state.peek() {
        match peek_tok.precedence() {
            Some(prec) => prec,
            None => <$precedence_type>::lowest(),
        }
    } else {
        <$precedence_type>::lowest()
    }
}

fn parse_grouped_expression<'a>(state: &mut ParserState<'a>) -> perr::ResOpt<'a, SpannedExpr<'a>> {
    let expr = parse_expression(state, <$precedence_type>::lowest())?;
    expect_peek_token(state, &$group_token_end)?;
    // consume group end token
    state.next().unwrap();
    Ok(expr)
}

fn expect_peek_token<'a>(
    state: &mut ParserState<'a>,
    expected: &$token_type
) -> perr::Result<'a, ()> {
    if let Some(&&ref peek_tok) = state.peek() {
        if peek_tok.spans_item(expected) {
            Ok(())
        } else {
            Err(perr::Error::spanned(
                perr::ErrorKind::Parse(format!("expected token {}, found token {}", expected,
                    peek_tok.item)),
                peek_tok.span
            ))
        }
    } else {
        Err(perr::Error::nospan(
            perr::ErrorKind::Parse(format!("expected token {} at end of input", expected))
        ))
    }
}

fn parse_prefix<'a>(state: &mut ParserState<'a>) -> perr::ResOpt<'a, SpannedExpr<'a>> {
    debug_assert!(state.peek().is_some());
    let &pspan::Spanned { span, item: ref token } = state.next().unwrap();
    match *token {
        // add all the literals as prefixes
        $(
            // from_token always returns Some(_) for Literals, unwrap is safe
            $literal_token(value) => Ok(Some(pspan::Spanned::new(Expression::Literal(
                Literal::from_token(&$literal_token(value)).unwrap()), span))),
        )*
        $group_token_start => Ok(parse_grouped_expression(state)?),
        $(
            $prefix_token => {
                //FIXME: I think this will break on a input-ending unary prefix operator
                match parse_expression(state, $prefix_precedence)? {
                    Some(right) => {
                        // FromToken is always defined at this repetition level, unwrap is safe
                        Ok(Some(pspan::Spanned::new(
                            Expression::Prefix {
                                op: PrefixOp::from_token(&$prefix_token).unwrap(),
                                right: Box::new(right),
                            },
                            span
                        )))
                    },
                    None => Ok(None),
                }
            },
        )*
        _ => Ok(None)
    }
}
fn parse_infix<'a>(state: &mut ParserState<'a>) -> perr::ResOpt<'a, (InfixOp, SpannedExpr<'a>)> {
    // if no peek token, we can't get here (due to peek_precedence), so unwrap is ok
    debug_assert!(state.peek().is_some());
    let &&ref spanned_tok = state.peek().unwrap();
    match spanned_tok.item {
        $(
            $infix_token => {
                // both Precedence and FromToken are defined above for this repeated clause,
                // so unwraps are safe
                let curr_precedence = spanned_tok.precedence().unwrap();
                state.next().unwrap(); // advance tokens
                let op = InfixOp::from_token(&$infix_token).unwrap();
                parse_expression(state, curr_precedence).map(|opt| opt.map(|expr| (op, expr)))
            }
        ,)*
        _ => Ok(None)
    }
}

mod _parse_statement {
    #[allow(unused_imports)]
    use super::*;

    $(
        #[allow(non_snake_case)]
        pub fn $statement_name<'a>(state: &mut ParserState<'a>)
                -> perr::ResOpt<'a, pspan::Spanned<'a, Statement<'a>>> {
            if let Some(&&pspan::Spanned { span: start_span, .. }) = state.peek() {
                statement_body!(state, $precedence_type; $($statement_tt)*);
                // there were enough tokens, and they all matched!
                Ok(Some(pspan::Spanned::new(
                    Statement::$statement_name(statement_binds!($($statement_tt)*)),
                    start_span
                )))
            } else {
                Ok(None) // end of input
            }
        }
    )*
}

    ); // end implementation macro expression arm
}


#[cfg(test)]
#[allow(dead_code)]
mod simple_calc {

    use span::Spanned;
    use lex::rules::{PTN_INT, convert_int};
    use parse::precedence::StandardPrecedence;

    lexer![
        r"\("                                   => LParen,
        r"\)"                                   => RParen,
        r"\+"                                   => Plus,
        r"\*"                                   => Asterisk,
        PTN_INT         => convert_int          => IntLiteral<i64>,
    ];

    parser![
        token_type: Token,
        group_tokens: (Token::LParen, Token::RParen),
        statements: [
            ExpressionStmt(expression<value>) := {expression<value>}
        ],
        literals: [
            Token::IntLiteral => Integer<i64>,
        ],
        precedence_type: StandardPrecedence,
        prefix<StandardPrecedence::Prefix>: [],
        infix: [
            Token::Asterisk => Multiply   => StandardPrecedence::Product,
            Token::Plus     => Add        => StandardPrecedence::Sum,
        ]
    ];


    #[test]
    fn test_simple_calc() {
        let tokens: Vec<Spanned<Token>> = lex("5 * (9 + 2)").unwrap();
        println!("{:?}", tokens);
        let ast = parse(&tokens);
        println!("{:?}", ast);

    }
}

#[cfg(test)]
#[allow(dead_code, unused_variables)]
mod statements {

    use span::Spanned;
    use parse::precedence::StandardPrecedence;
    use lex::rules::{PTN_INT, convert_int};

    lexer![
        r"\("                                   => LParen,
        r"\)"                                   => RParen,
        r"add"                                  => Add,
        r"to"                                   => To,
        PTN_INT         => convert_int          => IntLiteral<i64>,
    ];

    parser![
        token_type: Token,
        group_tokens: (Token::LParen, Token::RParen),
        statements: [
            OtherStmt(expression<value>, expression<operand>) :=
                {token<Token::Add> expression<value> token<Token::To> expression<operand>}
        ],
        literals: [
            Token::IntLiteral => Integer<i64>,
        ],
        precedence_type: StandardPrecedence,
        prefix<StandardPrecedence::Prefix>: [],
        infix: []
    ];

    #[test]
    fn test_statement() {
        let tokens: Vec<Spanned<Token>> = lex("add 5 to 9").unwrap();
        println!("{:?}", tokens);
        let ast = parse(&tokens);
        println!("{:?}", ast);
    }
}
