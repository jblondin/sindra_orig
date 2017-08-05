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
        r"="                                    => Equal,
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
            AssignmentStmt(identifier<name>, expression<value>) := {
                identifier<name> token<Token::Equal> expression<value>
            },
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
            Statement::AssignmentStmt((ident, expr)) => eval_assignment(ident, expr),
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

interp_repl!(lexer: lexer::lex, parser: parser::parse, evaluator: evaluator::Evaluator);

fn main() {
    start("SimpleCalc", "0.0.1");
}
