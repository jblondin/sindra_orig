#[macro_export]
macro_rules! statement_binds {
    (_cont identifier<$ident_name:ident>) => (
        $ident_name
    );
    (_cont expression<$expr_name:ident>) => (
        $expr_name
    );
    (_cont token<$tok:path>) => ();
    (_cont identifier<$ident_name:ident> $($statement_tt:tt)*) => (
        $ident_name, statement_binds!(_cont $($statement_tt)*)
    );
    (_cont expression<$expr_name:ident> $($statement_tt:tt)*) => (
        $expr_name, statement_binds!(_cont $($statement_tt)*)
    );
    (_cont token<$tok:path> $($statement_tt:tt)*) => (
        statement_binds!(_cont $($statement_tt)*)
    );
    (identifier<$ident_name:ident>) => (($ident_name));
    (expression<$expr_name:ident>) => (($expr_name));
    (token<$tok:path>) => ();
    (identifier<$ident_name:ident> $($statement_tt:tt)*) => (
        ($ident_name, statement_binds!(_cont $($statement_tt)*))
    );
    (expression<$expr_name:ident> $($statement_tt:tt)*) => (
        ($expr_name, statement_binds!(_cont $($statement_tt)*))
    );
    (token<$tok:path> $($statement_tt:tt)*) => (
        statement_binds!($($statement_tt)*)
    );
}

#[macro_export]
macro_rules! statement_args {
    (_cont identifier<$ident_name:ident>) => (
        pspan::Spanned<'a, Identifier>
    );
    (_cont expression<$expr_name:ident>) => (
        pspan::Spanned<'a, Expression<'a>>
    );
    (_cont identifier<$ident_name:ident>, $($statement_arg_tt:tt)*) => (
        pspan::Spanned<'a, Identifier>, statement_args!(_cont $($statement_arg_tt)*)
    );
    (_cont expression<$expr_name:ident>, $($statement_arg_tt:tt)*) => (
        pspan::Spanned<'a, Expression<'a>>, statement_args!(_cont $($statement_arg_tt)*)
    );
    (identifier<$ident_name:ident>) => ((pspan::Spanned<'a, Identifier>));
    (expression<$expr_name:ident>) => ((pspan::Spanned<'a, Expression<'a>>));
    (identifier<$ident_name:ident>, $($statement_arg_tt:tt)*) => (
        (pspan::Spanned<'a, Identifier>, statement_args!(_cont $($statement_arg_tt)*))
    );
    (expression<$expr_name:ident>, $($statement_arg_tt:tt)*) => (
        (pspan::Spanned<'a, Expression<'a>>, statement_args!(_cont $($statement_arg_tt)*))
    );
}

#[macro_export]
macro_rules! statement_body {
    ($state:ident, $prec_ty:ty; ) => (); // endpoint
    ($state:ident, $prec_ty:ty; identifier<$ident_name:ident> $($rest:tt)*) => (
        let $ident_name = {
            if $state.peek().is_some() {
                match parse_identifier($state) {
                    Ok(ident) => ident
                    Err(_) => { return Ok(None); }
                }
            } else {
                // input ended without finishing the match, return none
                return Ok(None);
            }
        };
        statement_body!($state, $prec_ty; $($rest)*);
    );
    ($state:ident, $prec_ty:ty; expression<$expr_name:ident> $($rest:tt)*) => (
        let $expr_name = {
            if $state.peek().is_some() {
                match parse_expression($state, <$prec_ty>::lowest()) {
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
        statement_body!($state, $prec_ty; $($rest)*);
    );
    ($state:ident, $prec_ty:ty; token<$tok:path> $($rest:tt)*) => (
        {
            if let Some(curr_tok) = $state.next() {
                if !curr_tok.spans_item(&$tok)  {
                    // token doesn't match pattern, return none
                    return Ok(None);
                }
            } else {
                // input ended without finishing the match, return none
                return Ok(None);
            }
        }
        statement_body!($state, $prec_ty; $($rest)*);
    );
}

/// Conditionally expands based on whether or not a repeating element has any members.
///
/// ```
/// # #[macro_use] extern crate sindra;
/// # fn main() {
/// macro_rules! foo (
///     ($($something:expr),*) => (
///         cond!(
///             if empty($($something)*) { "empty!" } else { "not empty!" }
///         )
///     );
/// );
/// assert_eq!("not empty!", foo!("hello ", "world"));
/// assert_eq!("empty!", foo!());
/// # }
#[macro_export]
macro_rules! cond {
    (if empty($($elem:tt)*) $if_block:block else $else_block:block) => (
        if_else_empty!(($($elem)*) >> $if_block, $else_block)
    );
    (if empty($($elem:tt)*) $if_block:block) => (
        if_else_empty!(($($elem)*) >> $if_block, {})
    );
    (if !empty($($elem:tt)*) $if_block:block else $else_block:block) => (
        if_else_empty!(($($elem)*) >> $else_block, $if_block)
    );
    (if !empty($($elem:tt)*) $if_block:block) => (
        if_else_empty!(($($elem)*) >> {}, $if_block)
    );
}

#[macro_export]
macro_rules! if_else_empty {
    (() >> $if_block:block, $else_block:block) => ($if_block);
    (($one:tt $($elem:tt)*) >> $if_block:block, $else_block:block) => ($else_block);
}
