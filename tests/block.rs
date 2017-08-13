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

use sindra::errors::{ErrorKind};

use self::evaluator::{Value};

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
    assert_value_matches!(in: TEST_INPUT_BASIC, expect: Value::Int(2));

    const TEST_INPUT_MISSINGVAR: &str = r#"
x = 5;
x
    "#;
    // 'x' should be updated inside the block
    assert_error!(in: TEST_INPUT_MISSINGVAR, match: "variable not found", err: ErrorKind::Store,
        start: Position::new(0, 2, 1), end: Some(Position::new(0, 2, 6)));

    const TEST_INPUT_OUTOFSCOPE: &str = r#"
let x = 5;
{
    let y = 2;
}
y
    "#;
    assert_error!(in: TEST_INPUT_OUTOFSCOPE, match: "identifier not found", err: ErrorKind::Eval,
        start: Position::new(0, 6, 1), end: Some(Position::new(0, 6, 2)));
}
