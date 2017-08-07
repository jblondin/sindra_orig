#![feature(plugin)]
#![plugin(sindra_plugin)]

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

    group_tokens![Token: Token::LParen, Token::RParen];
    block_tokens![Token: None];

    parser![
        token_type: Token,
        statements: [
            ExpressionStmt(expression<value>) := {expression<value>},
        ],
        literals: [
            Token::NumLiteral => Float<f64>,
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

    evaluator![
        program_type: Program,
        block_type: Block,
        identifier_type: Identifier,
        expression_type: Expression,
        values: [
            Literal::Float => Float<f64>
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

    fn add_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (_, _) => Err("addition only valid between two floats".to_string()),
        }
    }
    fn subtract_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            (_, _) => Err("subtraction only valid between two floats".to_string()),
        }
    }
    fn multiply_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (_, _) => Err("multiplication only valid between two floats".to_string()),
        }
    }
    fn divide_values(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            (_, _) => Err("division only valid between two floats".to_string()),
        }
    }
    fn raise(left: Value, right: Value) -> OpResult {
        match (left, right) {
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l.powf(r))),
            (_, _) => Err("raising only valid between two floats".to_string()),
        }
    }
    fn negate_value(right: Value) -> OpResult {
        match right {
            Value::Float(r) => Ok(Value::Float(-r)),
            _ => Err("unary negation only valid for floats".to_string())
        }
    }
    fn posate_value(right: Value) -> OpResult {
        match right {
            Value::Float(r) => Ok(Value::Float(r)),
            _ => Err("unary posation only valid for floats".to_string())
        }
    }

}

use self::evaluator::{Value};

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
