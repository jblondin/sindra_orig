#[macro_export]
macro_rules! statement_binds {
    (_list($($all:tt)*)) => (
        ($($all)*)
    );
    (_list($($all:tt)+) identifier<$ident_name:ident> $($statement_tt:tt)*) => (
        statement_binds!(_list($($all)* , $ident_name) $($statement_tt)*)
    );
    (_list($($all:tt)+) expression<$expr_name:ident> $($statement_tt:tt)*) => (
        statement_binds!(_list($($all)* , $expr_name) $($statement_tt)*)
    );
    (_list($($all:tt)+) token<$tok:path> $($statement_tt:tt)*) => (
        statement_binds!(_list($($all)*) $($statement_tt)*)
    );
    (_list() identifier<$ident_name:ident> $($statement_tt:tt)*) => (
        statement_binds!(_list($ident_name) $($statement_tt)*)
    );
    (_list() expression<$expr_name:ident> $($statement_tt:tt)*) => (
        statement_binds!(_list($expr_name) $($statement_tt)*)
    );
    (_list() token<$tok:path> $($statement_tt:tt)*) => (
        statement_binds!(_list() $($statement_tt)*)
    );
    ($($statement_tt:tt)*) => (
        statement_binds!(_list() $($statement_tt)*)
    );
}

#[macro_export]
macro_rules! statement_args {
    (_cont identifier<$ident_name:ident>) => (
        Spanned<'a, Identifier>
    );
    (_cont expression<$expr_name:ident>) => (
        Spanned<'a, Expression<'a>>
    );
    (_cont identifier<$ident_name:ident>, $($statement_arg_tt:tt)*) => (
        Spanned<'a, Identifier>, statement_args!(_cont $($statement_arg_tt)*)
    );
    (_cont expression<$expr_name:ident>, $($statement_arg_tt:tt)*) => (
        Spanned<'a, Expression<'a>>, statement_args!(_cont $($statement_arg_tt)*)
    );
    (identifier<$ident_name:ident>) => ((Spanned<'a, Identifier>));
    (expression<$expr_name:ident>) => ((Spanned<'a, Expression<'a>>));
    (identifier<$ident_name:ident>, $($statement_arg_tt:tt)*) => (
        (Spanned<'a, Identifier>, statement_args!(_cont $($statement_arg_tt)*))
    );
    (expression<$expr_name:ident>, $($statement_arg_tt:tt)*) => (
        (Spanned<'a, Expression<'a>>, statement_args!(_cont $($statement_arg_tt)*))
    );
}

#[macro_export]
macro_rules! statement_body {
    ($cursor:ident, $last_span:ident, $prec_ty:ty; ) => (); // endpoint
    ($cursor:ident, $last_span:ident, $prec_ty:ty; identifier<$ident_name:ident> $($rest:tt)*) => (
        let $ident_name = {
            if $cursor.has_next() {
                match parse_identifier($cursor) {
                    Ok(ident) => ident,
                    Err(_) => { return Ok(None); }
                }
            } else {
                // input ended without finishing the match, return none
                return Ok(None);
            }
        };
        $last_span = Some($ident_name.span);
        statement_body!($cursor, $last_span, $prec_ty; $($rest)*);
    );
    ($cursor:ident, $last_span:ident, $prec_ty:ty; expression<$expr_name:ident> $($rest:tt)*) => (
        let $expr_name = {
            if $cursor.has_next() {
                match parse_expression($cursor, <$prec_ty>::lowest()) {
                    Ok(expr_opt) => {
                        match expr_opt {
                            Some(expr) => expr,
                            None => { return Ok(None); }
                        }
                    },
                    Err(_) => { return Ok(None); }
                }
            } else {
                // input ended without finishing the match, return none
                return Ok(None);
            }
        };
        $last_span = Some($expr_name.span);
        statement_body!($cursor, $last_span, $prec_ty; $($rest)*);
    );
    ($cursor:ident, $last_span:ident, $prec_ty:ty; token<$tok:path> $($rest:tt)*) => (
        $last_span = {
            if let Some(curr_tok) = $cursor.next() {
                if !curr_tok.spans_item(&$tok)  {
                    // token doesn't match pattern, return none
                    return Ok(None);
                }
                Some(curr_tok.span)
            } else {
                // input ended without finishing the match, return none
                return Ok(None);
            }
        };
        statement_body!($cursor, $last_span, $prec_ty; $($rest)*);
    );
}
