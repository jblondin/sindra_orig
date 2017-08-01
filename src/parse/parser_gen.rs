//TODO:
// * remove reliance on precedence_type
#[macro_export]
macro_rules! parser_impl {
    (
        token_type($token_type:ty)
        group_tokens(($group_token_start:path, $group_token_end:path))
        identifier_token($identifier_token:path)
        statements([
            $($statement_name:tt($($statement_args_tt:tt)*)
                := { $($statement_tt:tt)* },)*
        ])
        literals([
            $($literal_token:path => $literal_name:ident<$literal_type:ty>,)*
        ])
        precedence_type($precedence_type:ty)
        prefix(($prefix_precedence:path, [
            $($prefix_token:path => $prefix_name:ident,)*
        ]))
        infix([
            $($infix_token:path => $infix_name:ident => $infix_precedence:expr,)*
        ])
        postfix(($postfix_precedence:path, [
            $($postfix_token:path => $postfix_name:ident,)*
        ]))
    ) => (

use $crate::errors;
use $crate::span::Spanned;
use $crate::pprint::PrettyPrint;

use $crate::parse::precedence::Lowest;

pub trait Precedence {
    type OpPrecedence: Lowest;

    fn precedence(&self) -> Option<Self::OpPrecedence>;
}

pub trait FromToken<T>: Sized {
    fn from_token(tok: &T) -> Option<Self>;
}

type SpannedToken<'a> = Spanned<'a, $token_type>;

pub type Program<'a> = Block<'a>;
pub type Block<'a> = Vec<SpannedStatement<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    $(
        $statement_name(statement_args!($($statement_args_tt)*))
    ),*
}
type SpannedStatement<'a> = Spanned<'a, Statement<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    $($literal_name($literal_type),)*
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
}
#[allow(dead_code)]
impl Identifier {
    pub fn new<T: AsRef<str>>(name: T) -> Identifier {
        Identifier { name: name.as_ref().to_string() }
    }
}
#[allow(dead_code)]
type SpannedIdentifier<'a> = Spanned<'a, Identifier>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal),
    #[allow(dead_code)]
    Identifier(Identifier),
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
type SpannedExpr<'a> = Spanned<'a, Expression<'a>>;

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

pub struct ParserCursor<'a> {
    state: ParserState<'a>,
    stash: Vec<ParserState<'a>>,
}
impl<'a> ParserCursor<'a> {
    fn next(&mut self) -> Option<&'a SpannedToken<'a>> {
        self.state.next()
    }
    fn has_next(&mut self) -> bool {
        self.state.peek().is_some()
    }
    fn peek(&mut self) -> Option<&&'a SpannedToken<'a>> {
        self.state.peek()
    }
    fn push_stash(&mut self) {
        self.stash.push(self.state.clone());
    }
    fn pop_stash(&mut self) {
        debug_assert!(!self.stash.is_empty());
        self.stash.pop();
    }
    fn restore_stash(&mut self) {
        debug_assert!(!self.stash.is_empty());
        if let Some(stash) = self.stash.pop() {
            self.state = stash;
        }
    }
}

pub fn parse<'a>(tokens: &'a Vec<SpannedToken<'a>>) -> errors::Result<'a, Program<'a>> {
    let mut cursor = ParserCursor { state: tokens.iter().peekable(), stash: vec![] };
    let mut program: Program = Program::new();
    while cursor.has_next() {
        let stmt = parse_statement(&mut cursor)?;
        program.push(stmt);
    }
    Ok(program)
}

fn parse_statement<'a>(cursor: &mut ParserCursor<'a>) -> errors::Result<'a, SpannedStatement<'a>> {
    debug_assert!(cursor.has_next());
    $(
        // save the stash so we can reverse if statement parse fails
        cursor.push_stash();
        match _parse_statement::$statement_name(cursor)? {
            Some(s) => {
                // we can get rid of the stashed parser state since this statement worked
                cursor.pop_stash();
                return Ok(s);
            },
            None => {
                // this didn't work, so restore the stash
                cursor.restore_stash();
                // continue on to try next statement
            }
        }
    )*

    // no statements matched
    // TOOD: improve error reporting here
    Err(errors::Error::nospan(errors::ErrorKind::Parse("no statement found".to_string())))
}

mod _parse_statement {
    #[allow(unused_imports)]
    use super::*;

    $(
        #[allow(non_snake_case)]
        pub fn $statement_name<'a>(cursor: &mut ParserCursor<'a>)
                -> errors::ResOpt<'a, Spanned<'a, Statement<'a>>> {
            if let Some(&&Spanned { span: start_span, .. }) = cursor.peek() {
                statement_body!(cursor, $precedence_type; $($statement_tt)*);
                // there were enough tokens, and they all matched!
                Ok(Some(Spanned::new(
                    Statement::$statement_name(statement_binds!($($statement_tt)*)),
                    start_span
                )))
            } else {
                Ok(None) // end of input
            }
        }
    )*
}

#[allow(dead_code)]
fn parse_identifier<'a>(
    cursor: &mut ParserCursor<'a>
) -> errors::Result<'a, SpannedIdentifier<'a>> {
    debug_assert!(cursor.has_next());
    let &Spanned { span, item: ref token } = cursor.next().unwrap();
    if let $identifier_token(ref ident_name) = *token {
        Ok(Spanned::new(Identifier::new(ident_name), span))
    } else {
        Err(errors::Error::spanned(errors::ErrorKind::Parse(
            "attempt to parse identifier from non-identifier token".to_string()), span))
    }
}

fn parse_expression<'a>(
    cursor: &mut ParserCursor<'a>,
    precedence: $precedence_type,
) -> errors::ResOpt<'a, SpannedExpr<'a>> {
    debug_assert!(cursor.has_next());
    let prefix_opt = parse_prefix(cursor)?;
    if prefix_opt.is_none() {
        return Ok(None);
    }

    cond!(
        if empty($($infix_token,)*) {
            Ok(Some(prefix_opt.unwrap()))
        } else {
            let mut left_expr = prefix_opt.unwrap();
            while precedence < peek_precedence(cursor) {
                let infix_expr = match parse_infix(cursor)? {
                    Some((op, right_expr)) => {
                        let left_span = left_expr.span;
                        Spanned::new(
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

fn peek_precedence<'a>(cursor: &mut ParserCursor<'a>) -> $precedence_type {
    // get the precedence of the next token (lowest if no token exists)
    if let Some(&&ref peek_tok) = cursor.peek() {
        match peek_tok.precedence() {
            Some(prec) => prec,
            None => <$precedence_type>::lowest(),
        }
    } else {
        <$precedence_type>::lowest()
    }
}

fn parse_grouped_expression<'a>(cursor: &mut ParserCursor<'a>)
        -> errors::ResOpt<'a, SpannedExpr<'a>> {
    let expr = parse_expression(cursor, <$precedence_type>::lowest())?;
    expect_peek_token(cursor, &$group_token_end)?;
    // consume group end token
    cursor.next().unwrap();
    Ok(expr)
}

fn expect_peek_token<'a>(
    cursor: &mut ParserCursor<'a>,
    expected: &$token_type
) -> errors::Result<'a, ()> {
    if let Some(&&ref peek_tok) = cursor.peek() {
        if peek_tok.spans_item(expected) {
            Ok(())
        } else {
            Err(errors::Error::spanned(
                errors::ErrorKind::Parse(format!("expected token {}, found token {}", expected,
                    peek_tok.item)),
                peek_tok.span
            ))
        }
    } else {
        Err(errors::Error::nospan(
            errors::ErrorKind::Parse(format!("expected token {} at end of input", expected))
        ))
    }
}

fn parse_prefix<'a>(cursor: &mut ParserCursor<'a>) -> errors::ResOpt<'a, SpannedExpr<'a>> {
    debug_assert!(cursor.has_next());
    let &Spanned { span, item: ref token } = cursor.next().unwrap();
    match *token {
        // add all the literals as prefixes
        $(
            // from_token always returns Some(_) for Literals, unwrap is safe
            $literal_token(ref value) => Ok(Some(Spanned::new(Expression::Literal(
                Literal::$literal_name(value.clone())), span))),
        )*
        // add identifier as prefixes
        $identifier_token(ref s) => Ok(Some(Spanned::new(Expression::Identifier(Identifier::new(s)),
            span))),
        $group_token_start => Ok(parse_grouped_expression(cursor)?),
        $(
            $prefix_token => {
                //FIXME: I think this will break on a input-ending unary prefix operator
                match parse_expression(cursor, $prefix_precedence)? {
                    Some(right) => {
                        // FromToken is always defined at this repetition level, unwrap is safe
                        Ok(Some(Spanned::new(
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
fn parse_infix<'a>(
    cursor: &mut ParserCursor<'a>
) -> errors::ResOpt<'a, (InfixOp, SpannedExpr<'a>)> {
    // if no peek token, we can't get here (due to peek_precedence), so unwrap is ok
    debug_assert!(cursor.has_next());
    let &&ref spanned_tok = cursor.peek().unwrap();
    match spanned_tok.item {
        $(
            $infix_token => {
                // both Precedence and FromToken are defined above for this repeated clause,
                // so unwraps are safe
                let curr_precedence = spanned_tok.precedence().unwrap();
                cursor.next().unwrap(); // advance tokens
                let op = InfixOp::from_token(&$infix_token).unwrap();
                parse_expression(cursor, curr_precedence).map(|opt| opt.map(|expr| (op, expr)))
            }
        ,)*
        _ => Ok(None)
    }
}

impl<'a> PrettyPrint for Statement<'a> {
    fn to_pp_string(&self) -> String {
        match *self {
            $(
                Statement::$statement_name(ref args) => {
                    format!("{}{}", stringify!($statement_name), args.to_pp_string())
                }
            ),*
        }
    }
}

impl<'a> PrettyPrint for Expression<'a> {
    fn to_pp_string(&self) -> String {
        match *self {
            Expression::Literal(ref lit) => lit.to_pp_string(),
            Expression::Identifier(ref ident) => ident.to_pp_string(),
            Expression::Infix { ref op, ref left, ref right } => {
                format!("{}({},{})", op.to_pp_string(), left.to_pp_string(), right.to_pp_string())
            },
            Expression::Prefix { ref op, ref right } => {
                format!("{}({})", op.to_pp_string(), right.to_pp_string())
            },
        }
    }
}

impl PrettyPrint for InfixOp {
    fn to_pp_string(&self) -> String {
        match *self {
            $(
                InfixOp::$infix_name => stringify!($infix_name).to_string()
            ),*
        }
    }
}

impl PrettyPrint for PrefixOp {
    fn to_pp_string(&self) -> String {
        match *self {
            $(
                PrefixOp::$prefix_name => stringify!($prefix_name).to_string()
            ),*
        }
    }
}

impl PrettyPrint for Literal {
    fn to_pp_string(&self) -> String {
        match *self {
            $(
                Literal::$literal_name(ref value) => format!("{}", value)
            ),*
        }
    }
}

impl PrettyPrint for Identifier {
    fn to_pp_string(&self) -> String {
        self.name.clone()
    }
}

    ); // end main macro implementation arm
}


#[macro_export]
macro_rules! parser {
    ($($all:tt)*) => (
        macro_preparse!(
            parser_impl,
            [
                token_type,
                group_tokens,
                identifier_token,
                statements,
                literals,
                precedence_type($crate::parse::precedence::StandardPrecedence),
                prefix(($crate::parse::precedence::StandardPrecedence::Prefix, [])),
                infix([]),
                postfix(($crate::parse::precedence::StandardPrecedence::Postfix, [])),
            ],
            $($all)*
        );
    );
}

#[cfg(test)]
#[allow(dead_code)]
mod simple_calc {

    mod lexer {
        use lex::rules::{
            PTN_INT, convert_int,
            PTN_IDENTIFIER, convert_identifier,
        };

        lexer![
            r"\("                                   => LParen,
            r"\)"                                   => RParen,
            r"\+"                                   => Plus,
            r"\*"                                   => Asterisk,
            PTN_INT         => convert_int          => IntLiteral<i64>,
            PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
        ];
    }

    mod parser {
        use super::lexer::Token;
        use parse::precedence::StandardPrecedence;

        parser![
            token_type: Token,
            group_tokens: (Token::LParen, Token::RParen),
            statements: [
                ExpressionStmt(expression<value>) := {expression<value>},
            ],
            literals: [
                Token::IntLiteral => Integer<i64>,
            ],
            identifier_token: Token::Identifier,
            precedence_type: StandardPrecedence,
            prefix: (StandardPrecedence::Prefix, []),
            infix: [
                Token::Asterisk => Multiply   => StandardPrecedence::Product,
                Token::Plus     => Add        => StandardPrecedence::Sum,
            ]
        ];
    }

    use span::Spanned;

    #[test]
    fn test_simple_calc() {
        let tokens: Vec<Spanned<lexer::Token>> = lexer::lex("5 * (9 + 2)").unwrap();
        println!("{:?}", tokens);
        let ast = parser::parse(&tokens);
        println!("{:?}", ast);

    }
}

#[cfg(test)]
#[allow(dead_code, unused_variables)]
mod statements {


    mod lexer {
        use lex::rules::{
            PTN_INT, convert_int,
            PTN_IDENTIFIER, convert_identifier,
        };
        lexer![
            r"\("                                   => LParen,
            r"\)"                                   => RParen,
            r"add"                                  => Add,
            r"to"                                   => To,
            PTN_INT         => convert_int          => IntLiteral<i64>,
            PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
        ];
    }

    mod parser {
        use super::lexer::Token;
        use parse::precedence::StandardPrecedence;

        parser![
            token_type: Token,
            group_tokens: (Token::LParen, Token::RParen),
            statements: [
                OtherStmt(expression<value>, expression<operand>) :=
                    {token<Token::Add> expression<value> token<Token::To> expression<operand>},
            ],
            literals: [
                Token::IntLiteral => Integer<i64>,
            ],
            identifier_token: Token::Identifier,
            precedence_type: StandardPrecedence,
            prefix: (StandardPrecedence::Prefix, []),
            infix: []
        ];
    }

    use span::Spanned;

    #[test]
    fn test_statement() {
        let tokens: Vec<Spanned<lexer::Token>> = lexer::lex("add 5 to 9").unwrap();
        println!("{:?}", tokens);
        let ast = parser::parse(&tokens);
        println!("{:?}", ast);
    }
}

