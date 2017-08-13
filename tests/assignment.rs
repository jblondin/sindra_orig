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

mod evaluator_return_assigned {

    evaluator![
        ast_module: super::parser,
        values: [
            Literal::Int => Int<i64>
        ],
        eval_statement: [
            Statement::AssignmentStmt((ident, expr)) =>
                eval_assignment(ident, expr, AssignReturn::Assigned),
            Statement::DeclarationStmt((ident, expr)) =>
                eval_declaration(ident, expr, Redeclare::Allowed, AssignReturn::Assigned),
            Statement::ExpressionStmt(expr) => eval_expression(expr)
        ],
    ];

    #[test]
    fn test_return() {
        let mut evaluator = Evaluator::new();
        // x initialize to 5
        assert_value_matches!(in: r#"let x = 5;"#, eval: evaluator, expect: Value::Int(5));
        // x should be set to 2 and 2 returned
        assert_value_matches!(in: r#"x = 2;"#, eval: evaluator, expect: Value::Int(2));
        // final value of 'x' should be 2
        assert_value_matches!(in: r#"x"#, eval: evaluator, expect: Value::Int(2));

    }

    #[test]
    fn test_redeclare() {
        let mut evaluator = Evaluator::new();
        // x initialize to 5
        assert_value_matches!(in: r#"let x = 5;"#, eval: evaluator, expect: Value::Int(5));
        // x should be redeclared as 6, with 6 returned
        assert_value_matches!(in: r#"let x = 6;"#, eval: evaluator, expect: Value::Int(6));
        // x should be set to 2 and 2 returned
        assert_value_matches!(in: r#"x = 2;"#, eval: evaluator, expect: Value::Int(2));
        // final value of 'x' should be 2
        assert_value_matches!(in: r#"x"#, eval: evaluator, expect: Value::Int(2));

    }
}

mod evaluator_return_prevvalue {

    evaluator![
        ast_module: super::parser,
        values: [
            Literal::Int => Int<i64>
        ],
        eval_statement: [
            Statement::AssignmentStmt((ident, expr)) =>
                eval_assignment(ident, expr, AssignReturn::PrevValue),
            Statement::DeclarationStmt((ident, expr)) =>
                eval_declaration(ident, expr, Redeclare::Allowed, AssignReturn::PrevValue),
            Statement::ExpressionStmt(expr) => eval_expression(expr)
        ],
    ];

    #[test]
    fn test_return() {
        let mut evaluator = Evaluator::new();
        // x is unset before initialization, so declaration should always return empty
        assert_value_matches!(in: r#"let x = 5;"#, eval: evaluator,
            expect: Value::Empty);
        // x should be set to 2 and 5 returned
        assert_value_matches!(in: r#"x = 2;"#, eval: evaluator, expect: Value::Int(5));
        // final value of 'x' should be 2
        assert_value_matches!(in: r#"x"#, eval: evaluator, expect: Value::Int(2));

    }

    #[test]
    fn test_redeclare() {
        let mut evaluator = Evaluator::new();
        // x is unset before initialization, so declaration should always return empty
        assert_value_matches!(in: r#"let x = 5;"#, eval: evaluator,
            expect: Value::Empty);
        // x should be redeclared as 6, with previous value 5 returned
        assert_value_matches!(in: r#"let x = 6;"#, eval: evaluator, expect: Value::Int(5));
        // x should be set to 2 and 6 returned
        assert_value_matches!(in: r#"x = 2;"#, eval: evaluator, expect: Value::Int(6));
        // final value of 'x' should be 2
        assert_value_matches!(in: r#"x"#, eval: evaluator, expect: Value::Int(2));

    }
}

mod evaluator_return_empty {

    evaluator![
        ast_module: super::parser,
        values: [
            Literal::Int => Int<i64>
        ],
        eval_statement: [
            Statement::AssignmentStmt((ident, expr)) =>
                eval_assignment(ident, expr, AssignReturn::Empty),
            Statement::DeclarationStmt((ident, expr)) =>
                eval_declaration(ident, expr, Redeclare::Allowed, AssignReturn::Empty),
            Statement::ExpressionStmt(expr) => eval_expression(expr)
        ],
    ];

    #[test]
    fn test_return() {
        let mut evaluator = Evaluator::new();
        // declaration should return empty
        assert_value_matches!(in: r#"let x = 5;"#, eval: evaluator, expect: Value::Empty);
        // assignment should return empty
        assert_value_matches!(in: r#"x = 2;"#, eval: evaluator, expect: Value::Empty);
        // final value of 'x' should be 2
        assert_value_matches!(in: r#"x"#, eval: evaluator, expect: Value::Int(2));
    }

    #[test]
    fn test_redeclare() {
        let mut evaluator = Evaluator::new();
        // declaration should return empty
        assert_value_matches!(in: r#"let x = 5;"#, eval: evaluator, expect: Value::Empty);
        // redeclaration should return empty
        assert_value_matches!(in: r#"let x = 6;"#, eval: evaluator, expect: Value::Empty);
        // assignment should return empty
        assert_value_matches!(in: r#"x = 2;"#, eval: evaluator, expect: Value::Empty);
        // final value of 'x' should be 2
        assert_value_matches!(in: r#"x"#, eval: evaluator, expect: Value::Int(2));

    }
}


mod evaluator_disallowed_redeclare {

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

    #[test]
    fn test_redeclare_error() {
        use sindra::errors::ErrorKind;

        let mut evaluator = Evaluator::new();
        // declaration should return empty
        assert_value_matches!(in: r#"let x = 5;"#, eval: evaluator, expect: Value::Int(5));
        // assignment should return empty
        assert_value_matches!(in: r#"x = 2;"#, eval: evaluator, expect: Value::Int(2));
        // final value of 'x' should be 2
        assert_value_matches!(in: r#"x"#, eval: evaluator, expect: Value::Int(2));
        // try to redefine 'x'
        assert_error!(in: r#"let x = 14;"#, eval: evaluator,
            match: "attempt to redeclare variable 'x'", err: ErrorKind::Eval,
            start: Position::new(0, 1, 5), end: Some(Position::new(0, 1, 11)));
    }
}
