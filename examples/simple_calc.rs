#![feature(plugin)]
#![plugin(sindra_plugin)]

#[macro_use] extern crate sindra;
extern crate regex;
extern crate rustyline;
extern crate clap;

mod lexer {
    use sindra::lex::rules::{
        PTN_NUM, convert_num,
        PTN_IDENTIFIER, convert_identifier,
        PTN_STRING, convert_string,
    };

    lexer![
        r"\("                                   => LParen,
        r"\)"                                   => RParen,
        r"\+"                                   => Plus,
        r"-"                                    => Minus,
        r"/"                                    => Slash,
        r"\*"                                   => Asterisk,
        r"\^"                                   => Caret,
        PTN_NUM         => convert_num          => NumLiteral<f64>,
        PTN_STRING      => convert_string       => StrLiteral<String>,
        PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
    ];
}

mod parser {
    use super::lexer::Token;
    use sindra::parse::precedence::StandardPrecedence;

    parser![
        token_type: Token,
        group_tokens: (Token::LParen, Token::RParen),
        literals: [
            Token::NumLiteral => Float<f64>,
            Token::StrLiteral => Str<String>,
        ],
        statements: [
            ExpressionStmt(expression<value>) := {expression<value>},
        ],
        identifier_token: Token::Identifier,
        precedence_type: StandardPrecedence,
        prefix: (StandardPrecedence::Prefix, [
            Token::Plus     => Plus,
            Token::Minus    => Minus,
        ]),
        infix: [
            Token::Plus     => Add        => StandardPrecedence::Sum,
            Token::Minus    => Subtract   => StandardPrecedence::Sum,
            Token::Asterisk => Multiply   => StandardPrecedence::Product,
            Token::Slash    => Divide     => StandardPrecedence::Product,
            Token::Caret    => Power      => StandardPrecedence::Power,
        ]
    ];
}

mod evaluator {
    use super::parser::{Program, Block, Statement, Identifier, Expression, InfixOp, PrefixOp,
        PostfixOp, Literal};
    use super::{multiply_values, add_values, subtract_values, divide_values, raise, negate_value,
        posate_value};

    evaluator![
        program_type: Program,
        block_type: Block,
        identifier_type: Identifier,
        expression_type: Expression,
        values: [
            Literal::Float => Float<f64>,
            Literal::Str   => String<String>
        ],
        eval_statement: [
            Statement::ExpressionStmt(expr) => eval_expression(expr)
        ],
        infix: (InfixOp, [
            InfixOp::Multiply => multiply_values,
            InfixOp::Add      => add_values,
            InfixOp::Subtract => subtract_values,
            InfixOp::Divide   => divide_values,
            InfixOp::Power    => raise
        ]),
        prefix: (PrefixOp, [
            PrefixOp::Minus   => negate_value,
            PrefixOp::Plus    => posate_value
        ]),
        postfix: (PostfixOp, [])
    ];
}

use self::evaluator::{Evaluator, Value};

fn add_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
        (_, _) => panic!("invalid add"),
    }
}
fn subtract_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
        (_, _) => panic!("invalid add"),
    }
}
fn multiply_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
        (_, _) => panic!("invalid multiply"),
    }
}
fn divide_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
        (_, _) => panic!("invalid add"),
    }
}
fn raise(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Float(l), Value::Float(r)) => Value::Float(l.powf(r)),
        (_, _) => panic!("invalid raise"),
    }
}
fn negate_value(right: Value) -> Value {
    match right {
        Value::Float(r) => Value::Float(-r),
        _ => panic!("invalid unary minus")
    }
}
fn posate_value(right: Value) -> Value {
    match right {
        Value::Float(r) => Value::Float(r),
        _ => panic!("invalid unary plus")
    }
}

interp_repl!(lexer: lexer::lex, parser: parser::parse, evaluator: Evaluator);

fn main() {
    start("SimpleCalc", "0.0.1");
}
