#[macro_use] extern crate sindra;
extern crate regex;

mod lexer {
    use sindra::lex::rules::{
        PTN_NUM, convert_num,
        PTN_IDENTIFIER, convert_identifier,
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
        PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
    ];
}

mod parser {
    use super::lexer::Token;
    use sindra::parse::precedence::StandardPrecedence;

    parser![
        token_type: Token,
        group_tokens: (Token::LParen, Token::RParen),
        statements: [
            ExpressionStmt(expression<value>) := {expression<value>},
        ],
        literals: [
            Token::NumLiteral => Float<f64>,
        ],
        identifier_token: Token::Identifier,
        precedence_type: StandardPrecedence,
        prefix<StandardPrecedence::Prefix>: [
            Token::Plus     => Plus,
            Token::Minus    => Minus,
        ],
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
        Expression::Infix { op, left, right }  => eval_infix(op, *left, *right, evaluator),
        Expression::Prefix { op, right }       => eval_prefix(op, *right, evaluator),
        Expression::Identifier(_)              => panic!("invalid identifier"),
    }
}
fn eval_literal(literal: Literal, _: &mut Evaluator) -> Value {
    match literal {
        Literal::Float(f)    => Value::Float(f),
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

fn assert_value_matches(s: &str, expected: Value) {
    let value = evaluator::eval(parser::parse(&lexer::lex(s).unwrap()).unwrap()).unwrap();
    assert_eq!(value, expected);
}

#[test]
fn test_infix() {
    assert_value_matches("5 + 3", Value::Float(8.0));
    assert_value_matches("5 + (3 * 4)", Value::Float(17.0));
    assert_value_matches("5 + 3 * 4", Value::Float(17.0));
    assert_value_matches("(5 + 3) * 4", Value::Float(32.0));
    assert_value_matches("5 / 2", Value::Float(2.5));
    assert_value_matches("4 / 2", Value::Float(2.0));
    assert_value_matches("5 - 2", Value::Float(3.0));
    assert_value_matches("5 - 2.0", Value::Float(3.0));
    assert_value_matches("5.0 - 2", Value::Float(3.0));
    assert_value_matches("5.0 - 2.0", Value::Float(3.0));
    assert_value_matches("5 + 3 / 4 - 2", Value::Float(3.75));
    assert_value_matches("(5 + 3) / (4 - 2)", Value::Float(4.0));
    assert_value_matches("5 - 2 ^ 4 * 1.5", Value::Float(-19.0));
    assert_value_matches("5 - 2 ^ (4 * 1.5)", Value::Float(-59.0));
    assert_value_matches("(5 - 2) ^ 4 * 1.5", Value::Float(121.5));
    assert_value_matches("(5 - 2) ^ (4 * 1.5)", Value::Float(729.0));
}

#[test]
fn test_prefix() {
    assert_value_matches("-5", Value::Float(-5.0));
    assert_value_matches("15 + -5", Value::Float(10.0));
    assert_value_matches("15 - -5", Value::Float(20.0));
    assert_value_matches("-5 - 4", Value::Float(-9.0));
    assert_value_matches("-5 + +4", Value::Float(-1.0));
    assert_value_matches("15 + +5", Value::Float(20.0));

}
