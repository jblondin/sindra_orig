use std::char;

use regex::{Regex, Captures};

use lex::errors::ErrorKind;
use lex::errors::rule::Result;

fn remove_underscores(input: &str) -> String {
    let mut s = String::new();
    for c in input.chars().filter(|&c| c != '_') { s.push(c); }
    s
}

// floating-point numbers
macro_rules! float_pattern {
    () => ( r"(?x)
        (?:[_0-9]+\.(?:[_0-9]+(?:[eE][\+-]?[_0-9]+)?)?) # with period, optional exponent
        |                                               # or
        (?:[_0-9]+[eE][\+-]?[_0-9]+)                    # no period, mandatory exponent
    " );
}
pub const PTN_FLOAT: &str = float_pattern!();
pub fn convert_float(whole: &str, _: &Captures) -> Result<f64> {
    let underscores_removed = remove_underscores(whole);
    underscores_removed.parse::<f64>().map_err(|e| ErrorKind::ConvertFloatError(e))
}

// integers (including hex / oct / bin representations)
macro_rules! int_pattern {
    () => ( r"(?x)
        0x(?P<hex>[_0-9A-Fa-f]+)   # hexadecimal
        |                          # or
        0o(?P<oct>[_0-8]+)         # octal
        |                          # or
        0b(?P<bin>[_0-1]+)         # binary
        |                          # or
        (?P<dec>[0-9][_0-9]*)      # decimal
    " );
}
pub const PTN_INT: &str = int_pattern!();
pub fn convert_int(whole: &str, captures: &Captures) -> Result<i64> {
    if let Some(mat) = captures.name("dec") {
        i64::from_str_radix(remove_underscores(mat.as_str()).as_str(), 10)
            .map_err(|e| ErrorKind::ConvertIntError(e))
    } else if let Some(mat) = captures.name("hex") {
        i64::from_str_radix(remove_underscores(mat.as_str()).as_str(), 16)
            .map_err(|e| ErrorKind::ConvertIntError(e))
    } else if let Some(mat) = captures.name("bin") {
        i64::from_str_radix(remove_underscores(mat.as_str()).as_str(), 2)
            .map_err(|e| ErrorKind::ConvertIntError(e))
    } else if let Some(mat) = captures.name("oct") {
        i64::from_str_radix(remove_underscores(mat.as_str()).as_str(), 8)
            .map_err(|e| ErrorKind::ConvertIntError(e))
    } else {
        Err(ErrorKind::ConvertStr(format!("string '{}' did not parse as integer", whole)))
    }
}

pub const PTN_NUM: &str = concat!(
    "(?P<float>", float_pattern!(), ")|(?P<int>", int_pattern!(), ")"
);
pub fn convert_num(whole: &str, captures: &Captures) -> Result<f64> {
    if let Some(_) = captures.name("float") {
        convert_float(whole, captures)
    } else if let Some(_) = captures.name("int") {
        convert_int(whole, captures).map(|i| i as f64)
    } else {
        Err(ErrorKind::ConvertStr(format!("string '{}' did not parse as number", whole)))
    }
}

// booleans
pub const PTN_BOOL: &str = r"(?P<true>true)|(?P<false>false)";
pub fn convert_bool(whole: &str, captures: &Captures) -> Result<bool> {
    if let Some(_) = captures.name("true") {
        Ok(true)
    } else if let Some(_) = captures.name("false") {
        Ok(false)
    } else {
        Err(ErrorKind::ConvertStr(format!("string '{}' did not parse as boolean", whole)))
    }
}

// string literals
macro_rules! unicode_char_pattern {
    () => { r#"\\u\{([[:xdigit:]]{1,6})\}"# };
}
lazy_static! {
    pub static ref UNICODE_REGEX: Regex = { Regex::new(unicode_char_pattern!()).unwrap() };
}

macro_rules! escape_pattern {
    () => { concat!(
        r#"(?xs)
        \\                                   # opening backslash
        [nrt\\0'"]                           # single-character escapes
        |                                    # or
        (?:\\x[[:xdigit:]]{2})               # one-byte escapes
        |                                    # or
        (?:"#, unicode_char_pattern!(), r")" // unicode escapes
    )};
}
lazy_static! {
    pub static ref ESCAPE_REGEX: Regex = { Regex::new(escape_pattern!()).unwrap() };
}

pub const PTN_STRING: &str = concat!(
    r#"(?xs)
    "                               # opening quote
    (?P<s>                          # main string capture group start

    (?:                             # repeatable character (or escape sequence) group
    (?:"#,                          // escape sequence non-capturing group start
    escape_pattern!(),              // escape sequences
    r#")                            # escape sequence non-capturing group end
    |                               # or
    [^"\\]                          # anything but a backslash or double quote
    )*                              # any number of characters / escape sequences

    )                               # main string capture group end
    "                               # closing quote
    "#
);
pub fn convert_string(whole: &str, captures: &Captures) -> Result<String> {
    // TODO FOR JULY 9: AARGH THIS ISN'T GOING TO WORK
    // I'm going to need to pass in CaptureMatches instead, or preferably .collect() the captures
    // returned by captures_iter() into my own collection to pass into rule methods so implementers
    // don't actually need to use the regex crate at all even if they implement their own converters
    // captures should always have capture group 0, this unwrap is safe

    if let Some(string_match) = captures.name("s") {
        let s = string_match.as_str();

        // 'result' will store final string (with all escapes replaced)
        let mut result = String::with_capacity(s.len());
        // 'offset' keeps track of position in matched text
        let mut offset = 0;
        // loop over rest of captures
        // re-run escape regex on string to find each escape sequence
        for cap in ESCAPE_REGEX.captures_iter(s) {
            // captures_iter will only yield matches, so this unwrap is ok
            let mat = cap.get(0).unwrap();
            // add everything up until this match to the result
            result.push_str(&s[offset..mat.start()]);
            // process the escape and add it to the result
            result.push_str(escape(mat.as_str())?.as_str());
            // update our position
            offset = mat.end();
        }
        // add everything after last match (or after string_match.start() if no matches) to result
        result.push_str(&s[offset..]);
        Ok(result)
    } else {
        Err(ErrorKind::ConvertStr(
            format!("string '{}' did not parse scan properly as string", whole)))
    }
}

fn escape(code: &str) -> Result<String> {
    let code_len = code.len();
    if code_len < 2 {
        return Err(ErrorKind::EscapeStr("invalid escape: empty".to_string()));
    }
    match &code[..2] {
        "\\n"     => Ok("\n".to_string()),
        "\\r"     => Ok("\r".to_string()),
        "\\t"     => Ok("\t".to_string()),
        "\\\\"    => Ok("\\".to_string()),
        "\\0"     => Ok("\0".to_string()),
        "\\'"     => Ok("'".to_string()),
        "\\\""    => Ok("\"".to_string()),
        "\\x"     => Ok(two_digit_escape(&code[2..])?),
        "\\u"     => {
            if let Some(caps) = UNICODE_REGEX.captures(code) {
                if let Some(mat) = caps.get(1) {
                    return Ok(unicode_escape(mat.as_str())?);
                }
            }
            Err(ErrorKind::EscapeStr(format!("improperly formatted unicode character code: {}",
                code)))
        },
        _         => Err(ErrorKind::EscapeStr(format!("unknown code {}", code))),
    }
}

fn two_digit_escape(code: &str) -> Result<String> {
    // two-digit character code escapes
    if code.len() != 2 {
        return Err(ErrorKind::EscapeStr(format!(
            "escape error with '\\{}': two-digit character code expected", code)));
    }
    let byte_code = u8::from_str_radix(code, 16).map_err(|e| ErrorKind::EscapeIntError(e))?;
    String::from_utf8(vec![byte_code]).map_err(|e| ErrorKind::EscapeUtf8Error(e.utf8_error()))
}

fn unicode_escape(code: &str) -> Result<String> {
    let unicode_value = u32::from_str_radix(code, 16).map_err(|e| ErrorKind::EscapeIntError(e))?;
    let c = char::from_u32(unicode_value).ok_or(ErrorKind::EscapeStr(
        format!("no character found for unicode value {}", unicode_value)))?;
    let mut s = String::new();
    s.push(c);
    Ok(s)
}


pub const PTN_IDENTIFIER: &str = r"\p{XID_Start}\p{XID_Continue}*";
pub fn convert_identifier(whole: &str, _: &Captures) -> Result<String> {
    Ok(whole.to_string())
}
