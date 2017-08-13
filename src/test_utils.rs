#[macro_export]
macro_rules! assert_value_matches {
    (
        in: $input:expr,
        expect: $expected:expr
    ) => ({
        assert_value_matches!(eval_mod: evaluator, in: $input, expect: $expected);
    });

    (
        lexer_mod: $lexer_mod:path,
        parser_mod: $parser_mod:path,
        in: $input:expr,
        expect: $expected:expr
    ) => ({
        assert_value_matches!(lexer_mod: $lexer_mod, parser_mod: $parser_mod, eval_mod: evaluator,
            in: $input, expect: $expected);
    });

    (
        eval_mod: $eval_mod:path,
        in: $input:expr,
        expect: $expected:expr
    ) => ({
        assert_value_matches!(lexer_mod: lexer, parser_mod: parser, eval_mod: $eval_mod,
            in: $input, expect: $expected);
    });

    (
        lexer_mod: $lexer_mod:path,
        parser_mod: $parser_mod:path,
        eval_mod: $eval_mod:path,
        in: $input:expr,
        expect: $expected:expr
    ) => ({
        use $eval_mod as eval_mod;
        let mut eval = eval_mod::Evaluator::new();
        assert_value_matches!(lexer_mod: $lexer_mod, parser_mod: $parser_mod, in: $input,
            eval: eval, expect: $expected);
    });

    (
        in: $input:expr,
        eval: $evaluator:ident,
        expect: $expected:expr
    ) => ({
        assert_value_matches!(lexer_mod: lexer, parser_mod: parser, in: $input, eval: $evaluator,
            expect: $expected);
    });

    (
        lexer_mod: $lexer_mod:path,
        parser_mod: $parser_mod:path,
        in: $input:expr,
        eval: $evaluator:ident,
        expect: $expected:expr
    ) => ({
        use $parser_mod as parser_mod;
        use $lexer_mod as lexer_mod;
        let value = $evaluator.eval(parser_mod::parse(&lexer_mod::lex($input).unwrap()).unwrap())
            .unwrap();
        println!("{:?}", value);
        assert_eq!(value, $expected);
    });
}

#[macro_export]
macro_rules! assert_error {
    (
        in: $input:expr,
        match: $match:expr,
        err: $err_type:path,
        start: $start_pos:expr,
        end: $end_pos:expr
    ) => ({
        assert_error!(eval_mod: evaluator, in: $input, match: $match, err: $err_type,
            start: $start_pos, end: $end_pos);
    });

    (
        lexer_mod: $lexer_mod:path,
        parser_mod: $parser_mod:path,
        in: $input:expr,
        match: $match:expr,
        err: $err_type:path,
        start: $start_pos:expr,
        end: $end_pos:expr
    ) => ({
        assert_error!(lexer_mod: $lexer_mod, parser_mod: $parser_mod, eval_mod: evaluator,
            in: $input, match: $match, err: $err_type, start: $start_pos, end: $end_pos);
    });

    (
        eval_mod: $eval_mod:path,
        in: $input:expr,
        match: $match:expr,
        err: $err_type:path,
        start: $start_pos:expr,
        end: $end_pos:expr
    ) => ({
        assert_error!(lexer_mod: lexer, parser_mod: parser, eval_mod: $eval_mod,
            in: $input, match: $match, err: $err_type, start: $start_pos, end: $end_pos);
    });

    (
        lexer_mod: $lexer_mod:path,
        parser_mod: $parser_mod:path,
        eval_mod: $eval_mod:path,
        in: $input:expr,
        match: $match:expr,
        err: $err_type:path,
        start: $start_pos:expr,
        end: $end_pos:expr
    ) => ({
        use $eval_mod as eval_mod;
        let mut eval = eval_mod::Evaluator::new();
        assert_error!(lexer_mod: $lexer_mod, parser_mod: $parser_mod, in: $input, eval: eval,
            match: $match, err: $err_type, start: $start_pos, end: $end_pos);
    });

    (
        in: $input:expr,
        eval: $evaluator:expr,
        match: $match:expr,
        err: $err_type:path,
        start: $start_pos:expr,
        end: $end_pos:expr
    ) => ({
        assert_error!(lexer_mod: lexer, parser_mod: parser, in: $input, eval: $evaluator,
            match: $match, err: $err_type, start: $start_pos, end: $end_pos);
    });

    (
        lexer_mod: $lexer_mod:path,
        parser_mod: $parser_mod:path,
        in: $input:expr,
        eval: $evaluator:expr,
        match: $match:expr,
        err: $err_type:path,
        start: $start_pos:expr,
        end: $end_pos:expr
    ) => ({
        use $lexer_mod as lexer_mod;
        use $parser_mod as parser_mod;
        use $crate::errors::Error;
        use $crate::span::{Span, Position};

        let tokens = lexer_mod::lex($input).unwrap();
        let evald = $evaluator.eval(parser_mod::parse(&tokens).unwrap());
        println!("evaluator result: {:?}", evald);
        match evald {
            Err(Error { origin: $err_type(errmsg), span: Some(Span { start, end, .. }) }) => {
                assert!(errmsg.contains($match));
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
