# Sindra

[![Build Status](https://travis-ci.org/jblondin/sindra.svg?branch=master)](https://travis-ci.org/jblondin/sindra)

Sindra is a library of compiler tools developed by [Jamie Blondin](https://github.com/jblondin) to
assist in quickly developing new programming languages. It is composed of a set of Rust macros which
expand into the lexer, parser, and evaluator components of a typical compiler pipeline.

Sindra currently just consists of a (very limited) lexer generator and parser generator. A
recusrive tree-walking evaluator which can produce an REPL and an interpreter is currently under
development. At some point in the future, there will ideally also be a compiler.

Additional tests, examples and documentation will be forthcoming.
