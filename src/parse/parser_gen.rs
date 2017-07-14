
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
        prefix: [
            $($prefix_token:path => $prefix_name:ident,)*
        ],
        infix<$precedence_type:ty>: [
            $($infix_token:path => $infix_name:ident => $infix_precedence:expr,)*
        ]
    ) => (


use std;

use $crate::parse::errors as perrors;

use $crate::parse::precedence::{Precedence, Lowest};
use $crate::parse::from_token::FromToken;

pub type Program = Block;
pub type Block = Vec<Statement>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    $(
        $statement_name(statement_args!($($statement_args_tt)*))
    ),*
}

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
pub enum Expression {
    Literal(Literal),
    Infix {
        op: InfixOp,
        left: Box<Expression>,
        right: Box<Expression>
    },
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

pub trait ParsePrefix{
    fn parse_prefix(&self, parser: &Parser, state: &mut ParserState)
        -> perrors::Result<Option<Expression>>;
}
impl ParsePrefix for $token_type {
    fn parse_prefix(&self,
        parser: &Parser,
        state: &mut ParserState
    ) -> perrors::Result<Option<Expression>> {
        match *self {
            // add all the literals as prefixes
            $(
                // from_token always returns Some(_) for Literals, unwrap is safe
                $literal_token(value) => Ok(Some(Expression::Literal(
                    Literal::from_token(&$literal_token(value)).unwrap()))),
            )*
            $group_token_start => Ok(parser.parse_grouped_expression(state)?),
            _ => Ok(None)
        }
    }
}

pub trait ParseInfix: Precedence {
    // parse_infix _must_ advance tokens if it returns Ok(Some(_))
    fn parse_infix(&self, left: Expression, parser: &Parser, state: &mut ParserState)
        -> perrors::Result<Option<Expression>>;
}
impl ParseInfix for $token_type {
    fn parse_infix(&self,
        left: Expression,
        parser: &Parser,
        state: &mut ParserState
    ) -> perrors::Result<Option<Expression>> {
        match *self {
            $(
                $infix_token => {
                    // both Precedence and FromToken are defined above for this repeated clause,
                    // so unwraps are safe
                    let curr_precedence = self.precedence().unwrap();
                    state.next().unwrap(); // advance tokens
                    match parser.parse_expression(state, curr_precedence)? {
                        Some(right) => {
                            Ok(Some(Expression::Infix {
                                op: InfixOp::from_token(&$infix_token).unwrap(),
                                left: Box::new(left),
                                right: Box::new(right),
                            }))
                        },
                        None => Ok(None)
                    }
                }
            ,)*
            _ => Ok(None)
        }
    }
}

impl Precedence for $token_type {
    type OpPrecedence = $precedence_type;

    fn precedence(&self) -> Option<Self::OpPrecedence> {
        InfixOp::from_token(self).and_then(|io| io.precedence())
    }
}

type StatementFuncs = Vec<fn(&Parser, &mut ParserState) -> perrors::Result<Option<Statement>>>;
type ExpressionFuncs = Vec<fn(&Parser, &mut ParserState) -> perrors::Result<Option<Expression>>>;

pub struct Parser {
    statements: StatementFuncs,
}
impl Parser {
    pub fn new() -> Parser {
        let mut statements: StatementFuncs = Vec::new();
        $(
            statements.push(_parse_statement::$statement_name);
        )*
        Parser {
            statements: statements,
        }
    }
    pub fn parse(&self, tokens: &Vec<$token_type>) -> perrors::Result<Program> {
        let mut state = tokens.iter().peekable();
        let mut program = Program::new();
        while state.peek().is_some() {
            program.push(self.parse_statement(&mut state)?);
        }
        Ok(program)
    }

    fn parse_statement(&self, state: &mut ParserState) -> perrors::Result<Statement> {
        debug_assert!(state.peek().is_some());
        for statement_func in &self.statements {
            match statement_func(self, state)? {
                Some(s) => {
                    return Ok(s);
                },
                None => { /* continue on to try next statement */ },
            }
        }
        Err(perrors::ErrorKind::Parse("no statement found".to_string()))
    }
    fn parse_expression(
        &self,
        state: &mut ParserState,
        precedence: $precedence_type,
    ) -> perrors::Result<Option<Expression>> {
        debug_assert!(state.peek().is_some());
        // this clone is potentially very costly, but necessary to avoid mutiple
        // mutable borrow of state.
        // TODO: this can be avoided by restructuring the code a bit here (and
        // modifying how ParseInfix works)
        let tok = state.next().unwrap().clone();
        let prefix_opt = tok.parse_prefix(&self, state)?;
        if prefix_opt.is_none() {
            return Ok(None);
        }

        cond!(
            if empty($($infix_token,)*) {
                Ok(Some(prefix_opt.unwrap()))
            } else {
                let mut left_expr = prefix_opt.unwrap();
                while precedence < peek_precedence(state) {
                    // if no peek token, we can't get here (due to peek_precedence), so unwrap is ok
                    debug_assert!(state.peek().is_some());

                    let infix_expr = {
                        // this clone is potentially very costly, but necessary to avoid mutiple
                        // mutable borrow of state.
                        // TODO: this can be avoided by restructuring the code a bit here (and
                        // modifying how ParseInfix works)
                        let peek_tok = state.peek().unwrap().clone();
                        // parse_infix is guaranteed to either advance tokens, or return None
                        let infix = peek_tok.parse_infix(left_expr.clone(), &self, state)?;
                        if infix.is_none() {
                            return Ok(Some(left_expr));
                        }
                        infix.unwrap()
                    };
                    left_expr = infix_expr;
                }
                Ok(Some(left_expr))
            }
        )
    }
    fn parse_grouped_expression(
        &self,
        state: &mut ParserState,
    ) -> perrors::Result<Option<Expression>> {
        let expr = self.parse_expression(state, <$precedence_type>::lowest())?;
        self.expect_peek_token(state, &$group_token_end)?;
        // consume group end token
        state.next().unwrap();
        Ok(expr)
    }
    fn expect_peek_token(
        &self,
        state: &mut ParserState,
        tok: &$token_type
    ) -> perrors::Result<()> {
        if let Some(&&ref peek_tok) = state.peek() {
            if peek_tok == tok {
                Ok(())
            } else {
                Err(perrors::ErrorKind::Parse(format!("expected token {}, found token {}",
                    tok, peek_tok)))
            }
        } else {
            Err(perrors::ErrorKind::Parse(format!("expected token {} at end of input", tok)))
        }
    }
}

pub type ParserState<'a> = std::iter::Peekable<std::slice::Iter<'a, $token_type>>;
fn peek_precedence(state: &mut ParserState) -> $precedence_type {
    // get the precedence of the next token (lowest if no token exists)
    // state.peek().and_then(|&pt| pt.precedence()).unwrap_or(P::lowest())
    if let Some(&&ref peek_tok) = state.peek() {
        match peek_tok.precedence() {
            Some(prec) => prec,
            None => <$precedence_type>::lowest(),
        }
    } else {
        <$precedence_type>::lowest()
    }
}
// pub struct ParserState<'a> {
//     iter: std::iter::Peekable<std::slice::Iter<'a, Token>>
// }
// impl<'a> ParserState<'a> {

// }

mod _parse_statement {
    #[allow(unused_imports)]
    use super::*;

    $(
        #[allow(non_snake_case)]
        pub fn $statement_name(
            parser: &Parser,
            state: &mut ParserState
        ) -> perrors::Result<Option<Statement>> {
            statement_body!(parser, state, $precedence_type; $($statement_tt)*);
            // there were enough tokens, and they all matched!
            Ok(Some(Statement::$statement_name(statement_binds!($($statement_tt)*))))
        }
    )*
}

    ); // end implementation macro expression arm
}


#[cfg(test)]
#[allow(dead_code)]
mod simple_calc {

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
        prefix: [],
        infix<StandardPrecedence>: [
            Token::Asterisk => Multiply   => StandardPrecedence::Product,
            Token::Plus     => Add        => StandardPrecedence::Sum,
        ]
    ];


    #[test]
    fn test_simple_calc() {
        let lexer = Lexer::new();
        let tokens = lexer.lex("5 * (9 + 2)").unwrap().iter()
            .map(|span| span.token.clone()).collect();
        println!("{:?}", tokens);
        let parser = Parser::new();
        let ast = parser.parse(&tokens);
        println!("{:?}", ast);

    }
}

#[cfg(test)]
#[allow(dead_code, unused_variables)]
mod statements {

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
        prefix: [],
        infix<StandardPrecedence>: []
    ];

    #[test]
    fn test_statement() {
        let lexer = Lexer::new();
        let tokens = lexer.lex("add 5 to 9").unwrap().iter()
            .map(|span| span.token.clone()).collect();
        println!("{:?}", tokens);
        let parser = Parser::new();
        let ast = parser.parse(&tokens);
        println!("{:?}", ast);
    }
}
