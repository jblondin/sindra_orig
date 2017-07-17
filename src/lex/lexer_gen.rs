
#[macro_export]
macro_rules! lexer {
    (
      direct [$($d_lexeme:expr => $d_token:ident,)*];
      rules [$($r_lexeme:expr => $r_rule:expr => $r_token:ident<$r_type:ty>,)*];
    ) => (

use std::fmt;

use regex::{Regex, RegexSet, Match, Captures};

use $crate::lex::errors as lex_errors;
use $crate::lex::span::{self, Position, Offset, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    $($d_token,)*
    $($r_token($r_type),)*
    Unrecognized
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //TODO: allow the user to provide their own names for the tokens
        write!(f, "{}", stringify!(self))
    }
}

pub type TokenSpan<'a> = span::TokenSpan<'a, Token>;

mod _token_funcs {
    #[allow(unused_imports)]
    use super::*;

    $(
        #[allow(non_snake_case)]
        pub mod $r_token {
            use super::*;

            pub fn parse<'a>(
                whole: &'a str,
                captures: &Captures
            ) -> lex_errors::lex_token::Result<'a, Token> {
                match $r_rule(whole, captures) {
                    Ok(value) => Ok(Token::$r_token(value)),
                    Err(e)    => Err(lex_errors::lex_token::Error::new(whole, e)),
                }
            }
        }
    )*
}

struct RegexList {
    regexes: Vec<Regex>,
}
impl RegexList {
    fn from_vec(vec: Vec<Regex>) -> RegexList {
        RegexList { regexes: vec }
    }
    fn find_with<'t>(&self, input: &'t str, index: usize) -> Option<Match<'t>> {
        let regex = &self.regexes[index];
        regex.find(input)
    }
    fn captures_with<'t>(&self, input: &'t str, index: usize) -> Option<Captures<'t>> {
        let regex = &self.regexes[index];
        regex.captures(input)
    }
    fn len(&self) -> usize {
        self.regexes.len()
    }
}

pub trait Lex {
    fn lex<'a>(&'a self) -> lex_errors::Result<'a, Vec<TokenSpan<'a>>>;
}
impl Lex for str {
    fn lex<'a>(&'a self) -> lex_errors::Result<'a, Vec<TokenSpan<'a>>> {
        Lexer::new(self).lex()
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    direct_tokens: Vec<Token>,
    rules: Vec<fn(&'a str, &Captures) -> lex_errors::lex_token::Result<'a, Token>>,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            direct_tokens: vec![$(Token::$d_token),*],
            rules: vec![$(_token_funcs::$r_token::parse),*],
        }
    }
    pub fn lex(&self) -> lex_errors::Result<'a, Vec<TokenSpan<'a>>> {
        // compile the regexes twice, once for when we need to match individual regexes, and once
        // in the regex set (which processes all at once)
        // we also anchor each expression so it only matches the beginning of the input
        let anchor_to_start = |s: &str| "^(?:".to_string() + s + ")";
        let regex_list = RegexList::from_vec(vec![
            $(Regex::new(&anchor_to_start($d_lexeme)).unwrap()),*,
            $(Regex::new(&anchor_to_start($r_lexeme)).unwrap()),*
        ]);
        let set = RegexSet::new([
            $(anchor_to_start($d_lexeme)),*,
            $(anchor_to_start($r_lexeme)),*
        ].iter()).unwrap();

        let mut curr_pos = Position::start();
        let mut tokens = vec![];
        let whitespace_devourer = Regex::new(r"^\s+").unwrap();
        let input_len = self.input.len();

        while curr_pos.byte < input_len {
            // eat some whitespace
            if let Some(mat) = whitespace_devourer.find(&self.input[curr_pos.byte..]) {
                debug_assert_eq!(mat.start(), 0); // anchored at begining of line
                curr_pos.offset(&Offset::from_str(mat.as_str()));
                if curr_pos.byte >= input_len {
                    break;
                }
            }

            // try to lex the next token
            let (token, matched) = self.lex_token(&self.input[curr_pos.byte..], &regex_list, &set)
                .map_err(|lte| lex_errors::ErrorKind::Spanned(lte,
                    Span::new(self.input, curr_pos)))?;
            let offset = Offset::from_str(matched);
            if offset.nbytes() == 0 {
                return Err(lex_errors::ErrorKind::WithSpan(
                    "Regex error: 0-length string matched".to_string(),
                    Span::new(self.input, curr_pos)));
            }
            let prev_pos = curr_pos;
            curr_pos.offset(&offset);
            tokens.push(TokenSpan::new(token,
                Span::new(&self.input[prev_pos.byte..curr_pos.byte], prev_pos)));
        }
        Ok(tokens)
    }

    fn lex_token(
        &self,
        input: &'a str,
        regex_list: &RegexList,
        set: &RegexSet)
    -> lex_errors::lex_token::Result<'a, (Token, &'a str)> {
        debug_assert!(input.len() > 0);

        // general algorithm:
        // 1. Go through regex set and find which ones match
        // 2. For variable-length regexes, compute the length of each match (prefer doing this
        //    during the regex set match, but it would be an effort to update the regex code,
        //    and we don't expect a significant number of matches).
        // 3. Use longest-matching regex

        // get indexes (of regex set) to matching regex
        let matches = set.matches(input).iter().collect::<Vec<_>>();
        let num_matching_regexes = matches.len();

        let (token, matched) = if num_matching_regexes == 0 {
            // move ahead one char to try and continue
            // since input is non-empty, unwrap is safe here
            (Token::Unrecognized, &input[..input.chars().next().unwrap().len_utf8()])
        } else {
            // determine which regex index to use; default to using the longest matches regex if
            // multiple regexes were matched
            let (token_index, matched_str) = if matches.len() == 1 {
                // only one matched, so we can go ahead and use it
                // unwraps here are ok because we already know it matched
                let token_index = *matches.first().unwrap();
                (token_index, regex_list.find_with(input, token_index).unwrap().as_str())
            } else {
                // multiple matches! find the longest match and use it
                let mut longest_len = 0;
                let mut longest_matchstr = "";
                let mut token_index = regex_list.len();
                for index in matches {
                    // unwrap here is ok because we already know it matched
                    // TODO(jblondin): for known-length expressions we don't need to compute
                    // length
                    let mat = regex_list.find_with(input, index).unwrap();
                    debug_assert_eq!(mat.start(), 0); // should be anchored at start
                    let len = mat.end();
                    if len > longest_len {
                        longest_len = len;
                        longest_matchstr = mat.as_str();
                        token_index = index;
                    }
                }
                (token_index, longest_matchstr)
            };

            // pull the appropriate token either from the direct or rule definition
            let num_direct_tokens = self.direct_tokens.len();
            let token = if token_index >= num_direct_tokens {
                // re-run regex to get captures
                // unwrap here ok because we already know it matched
                // TODO: see if it would be faster to just get captures originally
                let caps = regex_list.captures_with(input, token_index).unwrap();
                self.rules[token_index - num_direct_tokens](matched_str, &caps)?
            } else {
                self.direct_tokens[token_index].clone()
            };
            (token, matched_str)
        };
        Ok((token, matched))
    }
}

    ); // end implementation macro expression arm

    // match non-comma-terminated final direct definition
    (
        direct [$($d_lexeme:expr => $d_token:ident,)*];
        rules [$($r_lexeme:expr => $r_rule:expr => $r_token:ident<$r_type:ty>,)*];
        $new_d_lexeme:expr => $new_d_token:ident
    ) => (
        lexer!(
            direct [$($d_lexeme => $d_token,)* $new_d_lexeme => $new_d_token,];
            rules [$($r_lexeme => $r_rule => $r_token<$r_type>,)*];
        );
    );

    // match non-comma-terminated final rule definition
    (
        direct [$($d_lexeme:expr => $d_token:ident,)*];
        rules [$($r_lexeme:expr => $r_rule:expr => $r_token:ident<$r_type:ty>,)*];
        $new_r_lexeme:expr => $new_r_rule:expr => $new_r_token:ident<$new_r_type:ty>
    ) => (
        lexer!(
            direct [$($d_lexeme => $d_token,)*];
            rules [$($r_lexeme => $r_rule => $r_token<$r_type>,)*
                $new_r_lexeme => $new_r_rule => $new_r_token<$new_r_type>,];
        );
    );

    // add another direct definition
    (
        direct [$($d_lexeme:expr => $d_token:ident,)*];
        rules [$($r_lexeme:expr => $r_rule:expr => $r_token:ident<$r_type:ty>,)*];
        $new_d_lexeme:expr => $new_d_token:ident, $($rest:tt)*
    ) => (
        lexer!(
            direct [$($d_lexeme => $d_token,)* $new_d_lexeme => $new_d_token,];
            rules [$($r_lexeme => $r_rule => $r_token<$r_type>,)*];
            $($rest)*
        );
    );

    // add another rule definition
    (
        direct [$($d_lexeme:expr => $d_token:ident,)*];
        rules [$($r_lexeme:expr => $r_rule:expr => $r_token:ident<$r_type:ty>,)*];
        $new_r_lexeme:expr => $new_r_rule:expr => $new_r_token:ident<$new_r_type:ty>,
        $($rest:tt)*
    ) => (
        lexer!(
            direct [$($d_lexeme => $d_token,)*];
            rules [$($r_lexeme => $r_rule => $r_token<$r_type>,)*
                $new_r_lexeme => $new_r_rule => $new_r_token<$new_r_type>,];
            $($rest)*
        );
    );

    // start with a direct definition
    ( $d_lexeme:expr => $d_token:ident, $($rest:tt)* ) => (
        lexer!(
            direct [$d_lexeme => $d_token,];
            rules [];
            $($rest)*
        );
    );

    // start with a rule definition
    ( $r_lexeme:expr => $r_rule:expr => $r_token:ident<$r_type:ty>, $($rest:tt)* ) => (
        lexer!(
            direct [];
            rules [$r_lexeme => $r_rules => $r_token<$r_type>,];
            $($rest)*
        );
    );
} // end macro definition

#[cfg(test)]
mod basic {
    use lex::rules::{PTN_FLOAT, convert_float};

    lexer![
        r"\+"                               => Plus,
        r"\+="                              => PlusEqual,
        r"-"                                => Minus,
        r"-="                               => MinusEqual,
        r"="                                => Equal,
        r"=="                               => DoubleEqual,

        PTN_FLOAT       => convert_float    => FloatLiteral<f64>
    ];

    #[test]
    fn test_multimatch() {
        println!("{:?}", "+".lex());
        println!("{:?}", "+=".lex());
        println!("{:?}", "++=".lex());
        println!("{:?}", "    + += ++=".lex());
        println!("{:?}", "=

            +=+".lex());
    }
}
