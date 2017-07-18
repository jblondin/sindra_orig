#[macro_export]
macro_rules! interp_repl {
    (
        lexer: $lexer_type:ty,
        parser: $parser_type:ty,
        evaluator: $evaluator_type:ty
    ) => (

mod interp {
    use super::*;
    interp![lexer: $lexer_type, parser: $parser_type, evaluator: $evaluator_type];
}

mod repl {
    use super::*;
    repl![lexer: $lexer_type, parser: $parser_type, evaluator: $evaluator_type];
}

use clap::{Arg, App, ArgGroup};

pub fn start(lang_name: &str, version: &str) {
    let matches = App::new(format!("{} version {}", lang_name, version))
        .arg(Arg::with_name("c")
            .short("c")
            .help("Specifies the statement to run")
            .takes_value(true))
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to run"))
        .group(ArgGroup::with_name("input")
            .args(&["c", "INPUT"])
            .required(false))
        .get_matches();

    if let Some(statement) = matches.value_of("c") {
        interp::interp_string(statement);
    } else if let Some(input_file) = matches.value_of("INPUT") {
        interp::interp_file(input_file);
    } else {
        repl::start_repl(lang_name, version);
    }
    ::std::process::exit(0);

}

); // end of macro implementation

}
