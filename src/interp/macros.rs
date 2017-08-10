#[macro_export]
macro_rules! impl_read_eval_print {
    (
        $lexer_func:expr,
        $parser_func:expr,
        $self:tt
    ) => (

    fn read_eval_print(&mut $self, input: &str) -> Result {
        match $lexer_func(input) {
            Ok(tokenspans) => {
                if $self.print_tokens {
                    for token in &tokenspans {
                        writeln!($self.cout, "{:?}", token).map_err(|e| format!("{}: {}",
                            STDOUT_ERRSTR, e))?;
                    }
                }
                match $parser_func(&tokenspans) {
                    Ok(program) => {
                        if $self.print_ast {
                            writeln!($self.cout, "{:?}", program).map_err(|e| format!("{}: {}",
                                STDOUT_ERRSTR, e))?;
                        }
                        let value = $self.evaluator.eval(program);
                        match value {
                            Ok(v) => {
                                if $self.print_value {
                                    writeln!($self.cout, "{}", v).map_err(|e| format!("{}: {}",
                                        STDOUT_ERRSTR, e))?;
                                }
                            },
                            Err(e) => {
                                writeln!($self.cout, "{}", e).map_err(|e| format!("{}: {}",
                                    STDOUT_ERRSTR, e))?;
                            }
                        }
                    },
                    Err(e) => {
                        writeln!($self.cout, "{}", e).map_err(|e| format!("{}: {}", STDOUT_ERRSTR,
                            e))?
                    }
                }
            },
            Err(e) => {
                writeln!($self.cout, "{}", e).map_err(|e| format!("{}: {}", STDOUT_ERRSTR, e))?;
            }
        }
        Ok(())
    }
    )
}
