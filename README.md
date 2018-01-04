# An incomplete ANTLR4 grammar for Rust

The file Rust.g4 in this repository
is an [ANTLR4](http://www.antlr.org/) grammar
for [the Rust programming language](https://www.rust-lang.org/).

Note that the Rust repository contains an independent mostly-complete Yacc grammar:
[src/grammar/parser-lalr.y](https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y).

I wrote this grammar while I was working on
[*Programming Rust*](https://www.amazon.com/Programming-Rust-Fast-Systems-Development/dp/1491927283/),
to help make sure I understood the syntax.
Accordingly it has these limitations:

*   It's incomplete; many post-1.0 Rust features are missing.

*   It may be too permissive (“successfully” parsing text that isn’t valid Rust code),
    since I never did any negative testing.

*   The grammar may be ambiguous, because ANTLR4 makes no effort
    to reject ambiguous or buggy grammars.

*   It accepts some weird features that were experimental at the time,
    and were never officially part of the language.
    (My goal, never quite reached, was for it to parse every Rust file in the
    @rust-lang/rust repository, including tests for experimental features.)

## License

Like Rust itself, this grammar is distributed under the terms of both the MIT license and the Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
