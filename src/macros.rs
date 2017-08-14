
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
