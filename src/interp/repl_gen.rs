#[macro_export]
macro_rules! repl {
    (
        lexer: $lexer_func:expr,
        parser: $parser_func:expr,
        evaluator: $evaluator_type:ty
    ) => (

use std::io::Write;

use rustyline::{CompletionType, Editor};
use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;

mod result { pub type Result<T> = ::std::result::Result<T, String>; }
type Result = result::Result<()>;

struct Repl<W, E> {
    editor: Editor<FilenameCompleter>,

    cout: W,
    cerr: E,

    pub print_tokens: bool,
    pub print_ast: bool,
    pub print_value: bool,

    evaluator: $evaluator_type,
}

const HISTORY_FILE: &str = "repl_history.txt";
const REPL_CONFIG_PREFIX: &str = "/set";
const PROMPT: &str = ">> ";
const STDOUT_ERRSTR: &str = "unable to write to stdout";
const STDERR_ERRSTR: &str = "unable to write to stderr";

impl<W, E> Repl<W, E> where W: Write, E: Write {
    fn new(mut cout: W, cerr: E) -> Repl<W, E> {
        Repl {
            editor: {
                let config = rustyline::Config::builder()
                    .history_ignore_space(true)
                    .completion_type(CompletionType::List)
                    .build();
                let completer = FilenameCompleter::new();
                let mut editor = Editor::with_config(config);
                editor.set_completer(Some(completer));
                if editor.load_history(HISTORY_FILE).is_err() {
                    writeln!(cout, "No previous history.").expect(STDOUT_ERRSTR);
                }
                editor
            },

            cout: cout,
            cerr: cerr,

            print_tokens: false,
            print_ast: false,
            print_value: true,

            evaluator: <$evaluator_type>::new(),
        }
    }

    fn start(&mut self) -> Result {
        let mut running = true;
        while running {
            let line = self.editor.readline(PROMPT);
            match line {
                Ok(line) => {
                    self.editor.add_history_entry(line.as_ref());
                    if self.is_repl_command(&line) {
                        self.update_repl_config(&line)?;
                    } else {
                        self.read_eval_print(line)?;
                    }
                },
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    running = false;
                },
                Err(err) => {
                    writeln!(self.cerr, "Read error: {}", err)
                        .map_err(|e| format!("{}: {}", STDERR_ERRSTR, e))?;
                }
            }
        }
        self.editor.save_history(HISTORY_FILE)
            .map_err(|e| format!("Unable to save history: {}", e))?;
        Ok(())
    }

    fn is_repl_command(&mut self, s: &str) -> bool {
        s.to_lowercase().starts_with(REPL_CONFIG_PREFIX)
    }
    fn update_repl_config(&mut self, s: &str) -> Result {
        if s.len() <= REPL_CONFIG_PREFIX.len() {
            writeln!(self.cout, "invalid REPL command (missing config paramter): {}",
                REPL_CONFIG_PREFIX).map_err(|e| format!("{}: {}", STDOUT_ERRSTR, e))?;
            return Ok(());
        }
        match &s[REPL_CONFIG_PREFIX.len()..].trim().to_lowercase()[..] {
            "print_tokens on"  | "print_tokens true"   => { self.print_tokens = true; },
            "print_tokens off" | "print_tokens false"  => { self.print_tokens = false; },
            "print_ast on"     | "print_ast true"      => { self.print_ast = true; },
            "print_ast off"    | "print_ast false"     => { self.print_ast = false; },
            "print_value on"   | "print_value true"    => { self.print_value = true; },
            "print_value off"  | "print_value false"   => { self.print_value = false; },
            unknown => {
                writeln!(self.cout, "unknown REPL config parameter: {}", unknown)
                    .map_err(|e| format!("{}: {}", STDOUT_ERRSTR, e))?;
            }
        }
        Ok(())
    }


    fn read_eval_print(&mut self, input: String) -> Result {
        match $lexer_func(&input) {
            Ok(tokenspans) => {
                if self.print_tokens {
                    for token in &tokenspans {
                        writeln!(self.cout, "{:?}", token).map_err(|e| format!("{}: {}",
                            STDOUT_ERRSTR, e))?;
                    }
                }
                match $parser_func(&tokenspans) {
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

pub fn start_repl(lang_name: &str, version: &str) {
    println!("{} version {}", lang_name, version);
    let (stdout, stderr) = (::std::io::stdout(), ::std::io::stderr());
    let result = Repl::new(stdout.lock(), stderr.lock()).start();

    match result {
        Ok(_) => { ::std::process::exit(0); },
        Err(e) => {
            writeln!(::std::io::stderr(), "Error: {}", e).unwrap();
            ::std::process::exit(1);
        }
    }
}

    );
}
