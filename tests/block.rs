#![feature(plugin)]
#![plugin(sindra_plugin)]

#[macro_use] extern crate sindra;
extern crate regex;

mod lexer {
    use sindra::lex::rules::{
        PTN_INT, convert_int,
        PTN_IDENTIFIER, convert_identifier,
    };

    lexer![
        r"let"                                  => Let,
        r"\{"                                   => LBrace,
        r"\}"                                   => RBrace,
        r"="                                    => Equal,
        r";"                                    => Semicolon,
        PTN_INT         => convert_int          => IntLiteral<i64>,
        PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
    ];
}

mod parser {
    use super::lexer::Token;
    use sindra::parse::precedence::StandardPrecedence;

    group_tokens![Token: None];
    block_tokens![Token: Token::LBrace, Token::RBrace];
    identifier_token![Token: Token::Identifier, String];

    parser![
        token_type: Token,
        statements: [
            AssignmentStmt(identifier<name>, expression<value>) := {
                identifier<name> token<Token::Equal> expression<value> token<Token::Semicolon>
            },
            DeclarationStmt(identifier<name>, expression<value>) := {
                token<Token::Let> identifier<name> token<Token::Equal> expression<value>
                    token<Token::Semicolon>
            },
            ExpressionStmt(expression<value>) := {expression<value>},
        ],
        literals: [
            Token::IntLiteral => Int<i64>,
        ],
        precedence_type: StandardPrecedence,
        prefix: (StandardPrecedence::Prefix, []),
        infix: []
    ];
}


mod evaluator {

    evaluator![
        ast_module: super::parser,
        values: [
            Literal::Int => Int<i64>
        ],
        eval_statement: [
            Statement::AssignmentStmt((ident, expr)) =>
                eval_assignment(ident, expr, AssignReturn::Assigned),
            Statement::DeclarationStmt((ident, expr)) =>
                eval_declaration(ident, expr, Redeclare::Disallowed, AssignReturn::Assigned),
            Statement::ExpressionStmt(expr) => eval_expression(expr)
        ],
    ];

}

use sindra::errors::{Error, ErrorKind};
use sindra::span::{Span, Position};

use self::evaluator::{Value};

fn assert_value_matches(s: &str, expected: Value) {
    let value = evaluator::eval(parser::parse(&lexer::lex(s).unwrap()).unwrap()).unwrap();
    println!("{:?}", value);
    assert_eq!(value, expected);
}

macro_rules! assert_error {
    (
        $input:expr,
        $test:expr,
        $err_type:path,
        $start_pos:expr,
        $end_pos:expr
    ) => ({
        let tokens = lexer::lex($input).unwrap();
        let evald = evaluator::eval(parser::parse(&tokens).unwrap());
        println!("evaluator result: {:?}", evald);
        match evald {
            Err(Error { origin: $err_type(errmsg), span: Some(Span { start, end, .. }) }) => {
                assert!(errmsg.contains($test));
                assert!(start.row_col_eq(&$start_pos));
                let end_pos = $end_pos;
                match end {
                    Some(end) => {
                        assert!(end_pos.is_some() && end.row_col_eq(&end_pos.unwrap()));
                    },
                    None => { assert!(end_pos.is_none()); }
                }
            }
            other => { panic!("Expected error type {}, got: {:?}", stringify!($err_type), other); },
        }


    })
}

#[test]
fn test_scope() {
    const TEST_INPUT_BASIC: &str = r#"
let x = 5;
{
    x = 2;
}
x
    "#;
    // 'x' should be updated inside the block
    assert_value_matches(TEST_INPUT_BASIC, Value::Int(2));

    const TEST_INPUT_MISSINGVAR: &str = r#"
x = 5;
x
    "#;
    // 'x' should be updated inside the block
    assert_error!(TEST_INPUT_MISSINGVAR, "variable not found", ErrorKind::Store,
        Position::new(0, 2, 1), Some(Position::new(0, 2, 6)));

    const TEST_INPUT_OUTOFSCOPE: &str = r#"
let x = 5;
{
    let y = 2;
}
y
    "#;
    assert_error!(TEST_INPUT_OUTOFSCOPE, "identifier not found", ErrorKind::Eval,
        Position::new(0, 6, 1), Some(Position::new(0, 6, 2)));
}
