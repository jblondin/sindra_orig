#[macro_export]
macro_rules! lexer {
    (
      direct [$($d_lexeme:expr => $d_token:ident,)*];
      rules [$($r_lexeme:expr => $r_rule:expr => $r_token:ident<$r_type:ty>,)*];
    ) => (

use std::fmt;

use regex::{Regex, RegexSet, Match, Captures};

use $crate::errors;
use $crate::lex::errors as lex_errors;
use $crate::span::{Spanned, Position, Offset, Span};

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
            ) -> lex_errors::Result<'a, Token> {
                match $r_rule(whole, captures) {
                    Ok(value) => Ok(Token::$r_token(value)),
                    Err(e)    => Err(lex_errors::Error::new(whole, e)),
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

pub fn lex<'a>(input: &'a str) -> errors::MultiResult<'a, Vec<Spanned<'a, Token>>> {
    match Lexer::new(input).lex() {
        Ok(tokenspans) => {
            let mut lex_errors = vec![];
            let mut prev: Option<(usize, Span)> = None;
            for (i, sptok) in tokenspans.iter().enumerate() {
                if sptok.spans_item(&Token::Unrecognized) {
                    if let Some((prev_i, prev_span)) = prev {
                        if prev_i < i - 1 {
                            lex_errors.push(errors::Error::spanned(errors::ErrorKind::Lex(
                                format!("unrecognized token: '{}'", prev_span.as_slice())
                            ), prev_span));
                            prev = None;
                        } else {
                            prev = Some((i, prev_span.extend_to(&sptok.span)));
                        }
                    } else {
                        prev = Some((i, sptok.span));
                    }
                }
            }
            if let Some((_, final_span)) = prev {
                lex_errors.push(errors::Error::spanned(errors::ErrorKind::Lex(
                    format!("unrecognized token: '{}'", final_span.as_slice())
                ), final_span));
            }
            if lex_errors.is_empty() {
                Ok(tokenspans)
            } else {
                Err(errors::MultiError::new(format!("{} error(s) found", lex_errors.len()),
                    lex_errors))
            }
        },
        Err(e) => Err(errors::MultiError::new("lex error".to_string(), vec![e]))
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    direct_tokens: Vec<Token>,
    rules: Vec<fn(&'a str, &Captures) -> lex_errors::Result<'a, Token>>,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            direct_tokens: vec![$(Token::$d_token),*],
            rules: vec![$(_token_funcs::$r_token::parse),*],
        }
    }
    pub fn lex(&self) -> errors::Result<'a, Vec<Spanned<'a, Token>>> {
        // compile the regexes twice, once for when we need to match individual regexes, and once
        // in the regex set (which processes all at once)
        // we also anchor each expression so it only matches the beginning of the input
        #[allow(unused_variables)]
        let anchor_to_start = |s: &str| "^(?:".to_string() + s + ")";
        let regex_list = RegexList::from_vec(vec![
            $(Regex::new(&anchor_to_start($d_lexeme)).unwrap(),)*
            $(Regex::new(&anchor_to_start($r_lexeme)).unwrap()),*
        ]);
        let all_rules: Vec<String> = vec![
            $(anchor_to_start($d_lexeme),)*
            $(anchor_to_start($r_lexeme)),*
        ];
        let set = RegexSet::new(all_rules.iter()).unwrap();

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
                .map_err(|lte| errors::Error::spanned(errors::ErrorKind::LexToken(lte),
                    Span::new_from(&self.input, curr_pos)))?;
            let offset = Offset::from_str(matched);
            if offset.nbytes() == 0 {
                return Err(errors::Error::spanned(errors::ErrorKind::Lex(
                    "Regex error: 0-length string matched".to_string()),
                    Span::new_from(&self.input, curr_pos)));
            }
            let prev_pos = curr_pos;
            curr_pos.offset(&offset);
            tokens.push(Spanned::new(token,
                Span::new(&self.input, prev_pos, curr_pos)));
        }
        Ok(tokens)
    }

    fn lex_token(
        &self,
        input: &'a str,
        regex_list: &RegexList,
        set: &RegexSet)
    -> lex_errors::Result<'a, (Token, &'a str)> {
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
            rules [$r_lexeme => $r_rule => $r_token<$r_type>,];
            $($rest)*
        );
    );

    // empty
    () => (
        lexer!(
            direct [];
            rules [];
        );
    );
} // end macro definition

#[cfg(test)]
mod empty {
    mod lexer {
        lexer![];
    }

    #[test]
    fn test_empty() {
        let input = "";
        let tokens = self::lexer::lex(input).unwrap();
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn test_all_whitespace() {
        let input = "\t  \n";
        let tokens = self::lexer::lex(input).unwrap();
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn test_lex_unknown() {
        use span::{Span, Position};

        let input = "\n ☕";
        assert_unrecognized_token!(lexer_mod: self::lexer, in: input,
            expect_span: Span::new("\n ☕", Position::new(2, 2, 2), Position::new(5, 2, 3)));
    }

}

#[cfg(test)]
mod punctuation {
    mod lexer {
        lexer![
            r"\["   => LBracket,
            r"\]"   => RBracket,
            r"\("   => LParen,
            r"\)"   => RParen,
            r"\{"   => LBrace,
            r"\}"   => RBrace,
            r","    => Comma,
            r":"    => Colon,
            r";"    => Semicolon,
            r"\."   => Dot,
            r"\|"   => Pipe,
            r"~"    => Tilde,
            r"\?"    => Question,
        ];
    }

    #[test]
    fn test() {
        let input = r"[](){},:;.|~?";

        use self::lexer::Token;
        let expected = [
            Token::LBracket,
            Token::RBracket,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Colon,
            Token::Semicolon,
            Token::Dot,
            Token::Pipe,
            Token::Tilde,
            Token::Question
        ];

        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }
}

#[cfg(test)]
mod operators {
    mod lexer {
        lexer![
            r"="    => Equal,
            r"=="   => DoubleEqual,
            r"!"    => Not,
            r"!="   => NotEqual,
            r"<"    => LessThan,
            r"<="   => LessThanEqual,
            r">"    => GreaterThan,
            r">="   => GreaterThanEqual,
            r"\+"   => Plus,
            r"\+="  => PlusEqual,
            r"-"    => Minus,
            r"-="   => MinusEqual,
            r"\*"   => Asterisk,
            r"\*="  => AsteriskEqual,
            r"/"    => Slash,
            r"/="   => SlashEqual,
            r"\^"   => Caret,
            r"\^="  => CaretEqual,
        ];
    }


    #[test]
    fn test() {
        let input = "= == ! != < <= > >= + += - -= * *= / /= ^ ^=";

        use self::lexer::Token;
        let expected = [
            Token::Equal,
            Token::DoubleEqual,
            Token::Not,
            Token::NotEqual,
            Token::LessThan,
            Token::LessThanEqual,
            Token::GreaterThan,
            Token::GreaterThanEqual,
            Token::Plus,
            Token::PlusEqual,
            Token::Minus,
            Token::MinusEqual,
            Token::Asterisk,
            Token::AsteriskEqual,
            Token::Slash,
            Token::SlashEqual,
            Token::Caret,
            Token::CaretEqual,
        ];

        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }
}


#[cfg(test)]
mod keywords {
    mod lexer {
        lexer![
            r"let"      => Let,
            r"fn"       => Function,
            r"if"       => If,
            r"else"     => Else,
            r"return"   => Return,
        ];
    }

    #[test]
    fn test() {
        let input = "let fn if else return";

        use self::lexer::Token;
        let expected = [
            Token::Let,
            Token::Function,
            Token::If,
            Token::Else,
            Token::Return,
        ];

        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }
}

#[cfg(test)]
mod float {
    mod lexer {
        use lex::rules::{PTN_FLOAT, convert_float};

        lexer![
            PTN_FLOAT       => convert_float        => FloatLiteral<f64>,
        ];
    }

    #[test]
    fn test_lex_float_literal() {
        use self::lexer::Token;

        let input = "20. 10.249 10.249_942 1_234_567.890_100";
        let expected = [
            Token::FloatLiteral(20.0),
            Token::FloatLiteral(10.249),
            Token::FloatLiteral(10.249942),
            Token::FloatLiteral(1234567.8901)
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);

        let input = "20.1e2 20.2e+3 20.3e-4 20.4E5 20.5E+6 2_0.6E-7 20.0e8 20.9e1_0";
        let expected = [
            Token::FloatLiteral(2_010.),
            Token::FloatLiteral(20_200.),
            Token::FloatLiteral(0.00203),
            Token::FloatLiteral(2_040_000.),
            Token::FloatLiteral(20_500_000.),
            Token::FloatLiteral(0.00000206),
            Token::FloatLiteral(2_000_000_000.),
            Token::FloatLiteral(209_000_000_000.),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);

        let input = "2e5 4e-2";
        let expected = [
            Token::FloatLiteral(200_000.),
            Token::FloatLiteral(0.04),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }
}

#[cfg(test)]
mod int {
    mod lexer {
        use lex::rules::{PTN_INT, convert_int};

        lexer![
            PTN_INT         => convert_int          => IntLiteral<i64>,
        ];
    }

    use self::lexer::Token;

    #[test]
    fn test_lex_dec_literal() {
        let input = "1502 0000 052 1_234_567 1_2_3";
        let expected = [
            Token::IntLiteral(1502),
            Token::IntLiteral(0),
            Token::IntLiteral(52),
            Token::IntLiteral(1234567),
            Token::IntLiteral(1_2_3),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }

    #[test]
    fn test_lex_hex_literal() {
        let input = "0x1D02 0x000 0x052 0x1_234 0x1_C_3 0xDEADBEEF";
        const P16: [i64; 8] = [1, 16, 256, 4096, 65536, 1048576, 16777216, 268435456];
        let expected = [
            Token::IntLiteral(P16[3] + 13*P16[2] + 2),
            Token::IntLiteral(0),
            Token::IntLiteral(5*16 + 2),
            Token::IntLiteral(P16[3] + 2*P16[2] + 3*16 + 4),
            Token::IntLiteral(P16[2] + 12*16 + 3),
            Token::IntLiteral(13*P16[7] + 14*P16[6] + 10*P16[5]
                + 13*P16[4] + 11*P16[3] + 14*P16[2] + 14*P16[1] + 15*P16[0])
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }

    #[test]
    fn test_lex_oct_literal() {
        let input = "0o1502 0o000 0o052 0o1_234 0o1_2_3";
        const P8: [i64; 4] = [1, 8, 64, 512];
        let expected = [
            Token::IntLiteral(1*P8[3] + 5*P8[2] + 2),
            Token::IntLiteral(0),
            Token::IntLiteral(5*8 + 2),
            Token::IntLiteral(P8[3] + 2*P8[2] + 3*P8[1] + 4),
            Token::IntLiteral(P8[2] + 2*
                P8[1] + 3)
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);

        let input = "0o429";
        // interpreted as an octal number 42, then a decimal number 9
        let expected = [
            Token::IntLiteral(4*P8[1] + 2*P8[0]),
            Token::IntLiteral(9),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }

    #[test]
    fn test_lex_bin_literal() {
        let input = "0b101 0b001001 0b10_10";
        let expected = [
            Token::IntLiteral((1 << 2) + 1),
            Token::IntLiteral((1 << 3) + 1),
            Token::IntLiteral((1 << 3) + (1 << 1)),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }
}

#[cfg(test)]
mod num {
    mod lexer {
        use lex::rules::{PTN_NUM, convert_num};

        lexer![
            PTN_NUM         => convert_num          => NumLiteral<f64>,
        ];
    }

    use self::lexer::Token;

    #[test]
    fn test_float() {
        let input = "42.0 5.3 0.4 4.4e2";
        let expected = [
            Token::NumLiteral(42.0),
            Token::NumLiteral(5.3),
            Token::NumLiteral(0.4),
            Token::NumLiteral(440.0),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }

    #[test]
    fn test_int() {
        let input = "42 5 0 440";
        let expected = [
            Token::NumLiteral(42.0),
            Token::NumLiteral(5.0),
            Token::NumLiteral(0.0),
            Token::NumLiteral(440.0),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }
}

#[cfg(test)]
mod bool {
    mod lexer {
        use lex::rules::{PTN_BOOL, convert_bool};

        lexer![
            PTN_BOOL         => convert_bool          => BoolLiteral<bool>,
        ];
    }

    use self::lexer::Token;

    #[test]
    fn test_lex_bool_literal() {
        let input = "true false false";
        let expected = [
            Token::BoolLiteral(true),
            Token::BoolLiteral(false),
            Token::BoolLiteral(false),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);
    }

}

#[cfg(test)]
mod string {
    mod lexer {
        use lex::rules::{PTN_STRING, convert_string};

        lexer![
            PTN_STRING      => convert_string       => StringLiteral<String>,
        ];
    }

    use self::lexer::Token;
    use span::{Span, Position};
    use errors::{Error, MultiError, ErrorKind};
    use lex::errors::Error as LexError;
    use lex::errors::ErrorKind as LexErrorKind;

    #[test]
    fn test_lex_string_literal() {
        // basic strings and escapes
        let input = r#""hello there" "all you people ☺" "how\nare you \"doing\"?""#;
        let expected = [
            Token::StringLiteral("hello there".to_string()),
            Token::StringLiteral("all you people ☺".to_string()),
            Token::StringLiteral("how\nare you \"doing\"?".to_string()),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);

        // one-byte escapes
        let input = r#""\x41\x2D\x5A" "\x61\x2D\x7A" "\x30\x2D\x39""#;
        let expected = [
            Token::StringLiteral("A-Z".to_string()),
            Token::StringLiteral("a-z".to_string()),
            Token::StringLiteral("0-9".to_string()),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);

        // unicode escapes
        let input = r#" "\u{263A}\u{2639}" "\u{1F525}" "\u{1F37E}\u{1F37F}" "#;
        let expected = [
            Token::StringLiteral("☺☹".to_string()),
            Token::StringLiteral("🔥".to_string()),
            Token::StringLiteral("🍾🍿".to_string()),
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);

        //invalid escape codes
        let input = r#""\☺""#;
        assert_unrecognized_token!(lexer_mod: self::lexer, in: input,
            expect_span: Span::new(r#""\☺""#, Position::new(0, 1, 1), Position::new(6, 1, 5)));

        let input = r#""\q""#;
        assert_unrecognized_token!(lexer_mod: self::lexer, in: input,
            expect_span: Span::new(r#""\q""#, Position::new(0, 1, 1), Position::new(4, 1, 5)));

        let input = r#""\x4☺""#;
        assert_unrecognized_token!(lexer_mod: self::lexer, in: input,
            expect_span: Span::new(r#""\x4☺""#, Position::new(0, 1, 1), Position::new(8, 1, 7)));

        let input = r#""\xFF""#;
        match self::lexer::lex(input) {
            Ok(_) => { panic!("Expected lex error, but succeeded."); },
            Err(MultiError { errors, .. }) => {
                assert_eq!(errors.len(), 1);
                match errors[0] {
                    Error {
                        origin: ErrorKind::LexToken(LexError { ref kind, .. }),
                        span
                    } => {
                        match *kind {
                            LexErrorKind::EscapeUtf8Error(_) => {
                                assert_eq!(span, Some(Span::new_from(r#""\xFF""#,
                                    Position::new(0, 1, 1))));
                            },
                            ref e => {
                                panic!("Expected EscapeUtf8Error, found {:?}", e);
                            }
                        }
                    },
                    ref e => { panic!("Expected LexToken error, found {:?}", e); }
                }
            }
        }
    }

}

#[cfg(test)]
mod identifier {
    mod lexer {
        use lex::rules::{PTN_IDENTIFIER, convert_identifier};

        lexer![
            PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
        ];
    }

    use self::lexer::Token;
    use span::{Span, Position};

    #[test]
    fn test_lex_identifiers() {
        let input = "foo bar baz ª··9ℼ     ℝℨℤℤↈ９";
        let expected = [
            Token::Identifier("foo".to_string()),
            Token::Identifier("bar".to_string()),
            Token::Identifier("baz".to_string()),
            Token::Identifier("ª··9ℼ".to_string()), // ª is XID_Start, rest are XID_Continue
            Token::Identifier("ℝℨℤℤↈ９".to_string()), // ℝ is XID_Start, rest are XID_Continue
        ];
        assert_input_tokens!(lexer_mod: self::lexer, in: input, expect: expected);

        let input = " ９uise"; // ９ is XID_Continue but not XID_Start
        assert_unrecognized_token!(lexer_mod: self::lexer, in: input,
            expect_span: Span::new(" ９uise", Position::new(1, 1, 2), Position::new(4, 1, 3)));
    }
}

#[cfg(test)]
mod multiline {

    mod lexer {
        use lex::rules::{PTN_IDENTIFIER, convert_identifier};

        lexer![
            r"\+"                                   => Plus,
            r"="                                    => Equal,

            r","                                    => Comma,
            r";"                                    => Semicolon,

            r"\("                                   => LParen,
            r"\)"                                   => RParen,
            r"\{"                                   => LBrace,
            r"\}"                                   => RBrace,

            r"let"                                  => Let,
            r"fn"                                   => Function,

            PTN_IDENTIFIER  => convert_identifier   => Identifier<String>,
        ];
    }

    use self::lexer::Token;
    use span::{Span, Position, Spanned};

    #[test]
    fn test_multiline() {
        let input = "
let add = fn(x, y) {
    x + y;
};
        ";
        let new_sptok = |tok, start, end| {
            Spanned::new(tok, Span::new(&input, start, end))
        };
        let expected = [
            new_sptok(Token::Let, Position::new(1, 2, 1), Position::new(4, 2, 4)),
            new_sptok(Token::Identifier("add".to_string()), Position::new(5, 2, 5),
                Position::new(8, 2, 8)),
            new_sptok(Token::Equal, Position::new(9, 2, 9), Position::new(10, 2, 10)),
            new_sptok(Token::Function,  Position::new(11, 2, 11), Position::new(13, 2, 13)),
            new_sptok(Token::LParen, Position::new(13, 2, 13), Position::new(14, 2, 14)),
            new_sptok(Token::Identifier("x".to_string()), Position::new(14, 2, 14),
                Position::new(15, 2, 15)),
            new_sptok(Token::Comma, Position::new(15, 2, 15), Position::new(16, 2, 16)),
            new_sptok(Token::Identifier("y".to_string()), Position::new(17, 2, 17),
                Position::new(18, 2, 18)),
            new_sptok(Token::RParen, Position::new(18, 2, 18), Position::new(19, 2, 19)),
            new_sptok(Token::LBrace, Position::new(20, 2, 20), Position::new(21, 2, 21)),
            new_sptok(Token::Identifier("x".to_string()), Position::new(26, 3, 5),
                Position::new(27, 3, 6)),
            new_sptok(Token::Plus, Position::new(28, 3, 7), Position::new(29, 3, 8)),
            new_sptok(Token::Identifier("y".to_string()), Position::new(30, 3, 9),
                Position::new(31, 3, 10)),
            new_sptok(Token::Semicolon, Position::new(31, 3, 10), Position::new(32, 3, 11)),
            new_sptok(Token::RBrace, Position::new(33, 4, 1), Position::new(34, 4, 2)),
            new_sptok(Token::Semicolon, Position::new(34, 4, 2), Position::new(35, 4, 3)),
        ];


        assert_input_tokens!(match_span; lexer_mod: self::lexer, in: input, expect: expected);
    }
}
