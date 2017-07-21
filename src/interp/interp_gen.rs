#[macro_export]
macro_rules! interp {
    (
        lexer: $lex_func:expr,
        parser: $parse_func:expr,
        evaluator: $evaluator_type:ty
    ) => (

use clap::{Arg, App, ArgGroup};
use std::fs::File;

use std::io::{Write, Read};

mod result { pub type Result<T> = ::std::result::Result<T, String>; }
type Result = result::Result<()>;

struct Interpreter<W, E> {
    cout: W,
    #[allow(dead_code)]
    cerr: E,

    pub print_tokens: bool,
    pub print_ast: bool,
    pub print_value: bool,

    evaluator: $evaluator_type,
}

const STDOUT_ERRSTR: &str = "unable to write to stdout";

impl<W, E> Interpreter<W, E> where W: Write, E: Write {
    fn new(cout: W, cerr: E) -> Interpreter<W, E> {
        Interpreter {
            cout: cout,
            cerr: cerr,

            print_tokens: false,
            print_ast: false,
            print_value: true,

            evaluator: <$evaluator_type>::new(),
        }
    }

    pub fn process(&mut self, input: &str) -> Result {
        match $lex_func(input) {
            Ok(tokenspans) => {
                if self.print_tokens {
                    for token in &tokenspans {
                        writeln!(self.cout, "{:?}", token).map_err(|e| format!("{}: {}",
                            STDOUT_ERRSTR, e))?;
                    }
                }
                match $parse_func(&tokenspans) {
                    Ok(program) => {
                        if self.print_ast {
                            writeln!(self.cout, "{:?}", program).map_err(|e| format!("{}: {}",
                                STDOUT_ERRSTR, e))?;
                        }
                        let value = self.evaluator.eval(program);
                        if self.print_value {
                            writeln!(self.cout, "{:?}", value).map_err(|e| format!("{}: {}",
                                STDOUT_ERRSTR, e))?;
                        }
                    },
                    Err(e) => {
                        writeln!(self.cout, "{}", e).map_err(|e| format!("{}: {}", STDOUT_ERRSTR,
                            e))?
                    }
                }
            },
            Err(e) => {
                writeln!(self.cout, "{}", e).map_err(|e| format!("{}: {}", STDOUT_ERRSTR, e))?;
            }
        }
        Ok(())
    }
}

pub fn interp_string(s: &str) {
    let (stdout, stderr) = (::std::io::stdout(), ::std::io::stderr());
    let result = Interpreter::new(stdout.lock(), stderr.lock()).process(s);

    match result {
        Ok(_) => { ::std::process::exit(0); },
        Err(e) => {
            writeln!(::std::io::stderr(), "error: {}", e).unwrap();
            ::std::process::exit(1);
        }
    }
}

pub fn interp_file(file_name: &str) {
    match File::open(file_name) {
        Ok(mut file) => {
            let mut source = String::new();
            match file.read_to_string(&mut source) {
                Ok(_) => {
                    interp_string(&source);
                },
                Err(e) => {
                    writeln!(::std::io::stderr(), "file error: {}", e).unwrap();
                    ::std::process::exit(1);
                }
            }
        },
        Err(e) => {
            writeln!(::std::io::stderr(), "file error: {}", e).unwrap();
            ::std::process::exit(1);
        }
    }
}

#[allow(dead_code)]
pub fn start_interp(lang_name: &str, version: &str) {
    let matches = App::new(format!("{} version {}", lang_name, version))
        .arg(Arg::with_name("c")
            .short("c")
            .help("Specifies the statement to run")
            .takes_value(true))
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to run"))
        .group(ArgGroup::with_name("input")
            .args(&["c", "INPUT"])
            .required(true))
        .get_matches();

    if let Some(statement) = matches.value_of("c") {
        interp_string(statement);
    } else if let Some(input_file) = matches.value_of("INPUT") {
        interp_file(input_file);
    } else {
        writeln!(::std::io::stderr(), "{}", matches.usage()).unwrap();
        ::std::process::exit(1);
    }
    ::std::process::exit(0);
}

); // end of macro implementation

}
