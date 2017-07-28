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
    use super::parser::{Program, Block, Statement};
    use super::eval_expression;

    evaluator![
        program_type: Program,
        block_type: Block,
        identifier_type: String,
        values: [
            Float(f64)
        ],
        eval_statement: [
            Statement::ExpressionStmt((expr)) => eval_expression((expr))
        ]
    ];
}

use sindra::span::Spanned;
use self::parser::{Expression, Literal, InfixOp, PrefixOp};
use self::evaluator::{Evaluator, Value};

fn eval_expression(expr: (Spanned<Expression>), evaluator: &mut Evaluator) -> Value {
    match expr.item {
        Expression::Literal(literal)           => eval_literal(literal, evaluator),
        Expression::Identifier(_)              => Value::Empty,
        Expression::Infix { op, left, right }  => eval_infix(op, *left, *right, evaluator),
        Expression::Prefix { op, right }       => eval_prefix(op, *right, evaluator),
    }
}
fn eval_literal(literal: Literal, _: &mut Evaluator) -> Value {
    match literal {
        Literal::Float(f)    => Value::Float(f),
        Literal::Str(_)      => Value::Float(0.0),
    }
}
fn eval_infix(
    op: InfixOp,
    left: Spanned<Expression>,
    right: Spanned<Expression>,
    evaluator: &mut Evaluator
) -> Value {
    let left_value = eval_expression(left, evaluator);
    let right_value = eval_expression(right, evaluator);

    match op {
        InfixOp::Add      => add_values(left_value, right_value),
        InfixOp::Multiply => multiply_values(left_value, right_value),
        InfixOp::Subtract => subtract_values(left_value, right_value),
        InfixOp::Divide   => divide_values(left_value, right_value),
        InfixOp::Power    => raise(left_value, right_value),
    }
}
fn eval_prefix(
    op: PrefixOp,
    right: Spanned<Expression>,
    evaluator: &mut Evaluator
) -> Value {
    let right_value = eval_expression(right, evaluator);

    match op {
        PrefixOp::Minus      => negate_value(right_value),
        PrefixOp::Plus       => posate_value(right_value),
    }
}
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
