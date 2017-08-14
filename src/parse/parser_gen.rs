#[macro_export]
macro_rules! group_tokens {
    ($token_type:ty: None) => (

impl $token_type {
    fn group_token_start() -> Option<Self> { None }
    fn group_token_end() -> Option<Self> { None }
}

    );
    ($token_type:ty: $group_token_start:path, $group_token_end:path) => (

impl $token_type {
    fn group_token_start() -> Option<Self> { Some($group_token_start) }
    fn group_token_end() -> Option<Self> { Some($group_token_end) }
}

    );
}

#[macro_export]
macro_rules! block_tokens {
    ($token_type:ty: None) => (

impl $token_type {
    fn block_token_start() -> Option<Self> { None }
    fn block_token_end() -> Option<Self> { None }
}

    );
    ($token_type:ty: $block_token_start:path, $block_token_end:path) => (

impl $token_type {
    fn block_token_start() -> Option<Self> { Some($block_token_start) }
    fn block_token_end() -> Option<Self> { Some($block_token_end) }
}

    );
}

#[macro_export]
macro_rules! identifier_token {
    ($token_type:ty: None) => (

impl $token_type {
    fn is_identifier_token(&self) -> bool { false }
    fn extract_identifier(&self) -> Option<String> { None }
}

    );
    ($token_type:ty: $identifier_token:path) => (

impl $token_type {
    fn is_identifier_token(&self) -> bool {
        match *self {
            $identifier_token(_) => true,
            _ => false,
        }
    }
    fn extract_identifier(&self) -> Option<String> {
        match *self {
            $identifier_token(ref ident_name) => Some(ident_name.to_string()),
            _ => None,
        }
    }
}

    );
}

#[macro_export]
macro_rules! parser_impl {
    (
        token_type($token_type:ty)
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

use std::fmt;
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

pub trait IntoExpr<'a> {
    fn into_expr(self) -> Expression<'a>;
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
impl fmt::Display for Literal {
    #[allow(unused_variables)]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            $(
                Literal::$literal_name(ref value) => {
                    write!(f, "{}({})", stringify!($literal_name), value)
                }
            ),*
        }
    }
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
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name)
    }
}
#[allow(dead_code)]
type SpannedIdentifier<'a> = Spanned<'a, Identifier>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal),
    #[allow(dead_code)]
    Identifier(Identifier),
    #[allow(dead_code)]
    Infix {
        op: InfixOp,
        left: Box<SpannedExpr<'a>>,
        right: Box<SpannedExpr<'a>>
    },
    #[allow(dead_code)]
    Prefix {
        op: PrefixOp,
        right: Box<SpannedExpr<'a>>,
    },
    #[allow(dead_code)]
    Postfix {
        op: PostfixOp,
        left: Box<SpannedExpr<'a>>,
    },
    #[allow(dead_code)]
    Block(Block<'a>),
}
type SpannedExpr<'a> = Spanned<'a, Expression<'a>>;

impl<'a> IntoExpr<'a> for Block<'a> {
    fn into_expr(self) -> Expression<'a> {
        Expression::Block(self)
    }
}
impl<'a> IntoExpr<'a> for Literal {
    fn into_expr(self) -> Expression<'a> {
        Expression::Literal(self)
    }
}
impl<'a> IntoExpr<'a> for Identifier {
    fn into_expr(self) -> Expression<'a> {
        Expression::Identifier(self)
    }
}

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
impl fmt::Display for PrefixOp {
    #[allow(unused_variables)]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            $(
                PrefixOp::$prefix_name =>  write!(f, "{}", stringify!($prefix_name))
            ),*
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
impl fmt::Display for InfixOp {
    #[allow(unused_variables)]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            $(
                InfixOp::$infix_name =>  write!(f, "{}", stringify!($infix_name))
            ),*
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum PostfixOp {
    $($postfix_name,)*
}
impl FromToken<$token_type> for PostfixOp {
    fn from_token(tok: &$token_type) -> Option<PostfixOp> {
        match *tok {
            $($postfix_token => Some(PostfixOp::$postfix_name),)*
            _ => None
        }
    }
}
impl fmt::Display for PostfixOp {
    #[allow(unused_variables)]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            $(
                PostfixOp::$postfix_name =>  write!(f, "{}", stringify!($postfix_name))
            ),*
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
    fn is_peek_expected(&mut self, expected: &$token_type) -> bool {
        if let Some(&&ref peek_tok) = self.peek() {
            peek_tok.spans_item(expected)
        } else {
            false
        }
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
        #[allow(non_snake_case, unused_assignments)]
        pub fn $statement_name<'a>(cursor: &mut ParserCursor<'a>)
                -> errors::ResOpt<'a, Spanned<'a, Statement<'a>>> {
            if let Some(&&Spanned { span: start_span, .. }) = cursor.peek() {
                let mut last_span = None;
                statement_body!(cursor, last_span, $precedence_type; $($statement_tt)*);
                // there were enough tokens, and they all matched!
                Ok(Some(Spanned::new(
                    Statement::$statement_name(statement_binds!($($statement_tt)*)),
                    match last_span {
                        Some(s) => start_span.extend_to(&s),
                        None => start_span
                    }

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

    if let Some(ref ident_name) = token.extract_identifier() {
    // if let $identifier_token(ref ident_name) = *token {
        Ok(Spanned::new(Identifier::new(ident_name), span))
    } else {
        Err(errors::Error::spanned(errors::ErrorKind::Parse(
            "attempt to parse identifier from non-identifier token".to_string()), span))
    }
}

#[allow(unused_variables)]
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
                        let right_span = right_expr.span;
                        Spanned::new(
                            Expression::Infix {
                                op: op,
                                left: Box::new(left_expr),
                                right: Box::new(right_expr),
                            },
                            left_span.extend_to(&right_span)
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

#[allow(dead_code)]
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
    // parse_grouped_expression only called if group_token_* returns Some, this unwrap is safe
    expect_peek_token(cursor, &<$token_type>::group_token_end().unwrap())?;
    // consume group end token
    cursor.next().unwrap();
    Ok(expr)
}

fn parse_block<'a>(mut cursor: &mut ParserCursor<'a>) -> errors::ResOpt<'a, SpannedExpr<'a>> {
    let mut block = Block::new();
    // parse_block only called if block_token_* returns Some, unwraps are safe
    let start_span = if cursor.has_next() {
        cursor.peek().unwrap().span
    } else {
        return Err(errors::Error::nospan(errors::ErrorKind::Parse(
            format!("expected block end token {}, but end of input reached",
                <$token_type>::block_token_end().unwrap())
        )));
    };
    // parse_block only called if group_tokens() is Some, unwrap is safe
    while cursor.has_next() && !cursor.is_peek_expected(
            &<$token_type>::block_token_end().unwrap()) {
        let stmt = parse_statement(&mut cursor)?;
        block.push(stmt);
    }
    expect_peek_token(cursor, &<$token_type>::block_token_end().unwrap())?;
    // consume block end token
    let &Spanned {span: end_span, ..} = cursor.next().unwrap();
    let full_span = start_span.extend_to(&end_span);
    Ok(Some(Spanned::new(Expression::Block(block), full_span)))
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

#[allow(dead_code)]
fn parse_prefix<'a>(cursor: &mut ParserCursor<'a>) -> errors::ResOpt<'a, SpannedExpr<'a>> {
    debug_assert!(cursor.has_next());
    let &Spanned { span, item: ref token } = cursor.next().unwrap();
    match *token {
        // add all the literals as prefixes
        $(
            // from_token always returns Some(_) for Literals, unwrap is safe
            $literal_token(ref lit) => Ok(Some(Spanned::new(Expression::Literal(
                Literal::$literal_name(lit.clone())), span))),
        )*
        // add identifier as prefixes
        ref ident_token if ident_token.is_identifier_token() => {

            if let Some(ref ident_name) = token.extract_identifier() {
                Ok(Some(Spanned::new(Expression::Identifier(Identifier::new(ident_name)), span)))
            } else {
                Err(errors::Error::spanned(errors::ErrorKind::Parse(
                    "attempt to parse identifier from non-identifier token".to_string()), span))
            }
        },
        ref tok if Some(tok) == <$token_type>::group_token_start().as_ref()
            => Ok(parse_grouped_expression(cursor)?),
        ref tok if Some(tok) == <$token_type>::block_token_start().as_ref()
            => Ok(parse_block(cursor)?),
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

#[allow(dead_code)]
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
            Expression::Postfix { ref op, ref left } => {
                format!("{}({})", op.to_pp_string(), left.to_pp_string())
            },
            Expression::Block(ref block) => {
                block.to_pp_string()
            }
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

impl PrettyPrint for PostfixOp {
    fn to_pp_string(&self) -> String {
        match *self {
            $(
                PostfixOp::$postfix_name => stringify!($postfix_name).to_string()
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
                statements,
                literals([]),
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
        };

        lexer![
            r"\("                                   => LParen,
            r"\)"                                   => RParen,
            r"\+"                                   => Plus,
            r"\*"                                   => Asterisk,
            PTN_INT         => convert_int          => IntLiteral<i64>,
        ];
    }

    mod parser {
        use super::lexer::Token;
        use parse::precedence::StandardPrecedence;

        group_tokens![Token: Token::LParen, Token::RParen];
        block_tokens![Token: None];
        identifier_token![Token: None];

        parser![
            token_type: Token,
            statements: [
                ExpressionStmt(expression<value>) := {expression<value>},
            ],
            literals: [
                Token::IntLiteral => Integer<i64>,
            ],
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
            r"\{"                                   => LBrace,
            r"\}"                                   => RBrace,
            r"add"                                  => Add,
            r"to"                                   => To,
            PTN_INT         => convert_int          => IntLiteral<i64>,
            PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
        ];
    }

    mod parser {
        use super::lexer::Token;
        use parse::precedence::StandardPrecedence;

        group_tokens![Token: Token::LParen, Token::RParen];
        block_tokens![Token: Token::LBrace, Token::RBrace];
        identifier_token![Token: Token::Identifier];

        parser![
            token_type: Token,
            statements: [
                OtherStmt(expression<value>, expression<operand>) :=
                    {token<Token::Add> expression<value> token<Token::To> expression<operand>},
            ],
            literals: [
                Token::IntLiteral => Integer<i64>,
            ],
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

