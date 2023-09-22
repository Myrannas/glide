pub(crate) mod ast;
mod hand_parser;
mod lexer;
pub(crate) mod statements;
mod strings;

use crate::parser::hand_parser::{pretty_print, Parse, WhitespaceTrackingLexer};
use crate::result::{Result, SyntaxError};
use logos::Logos;
use std::collections::VecDeque;

pub use ast::ParsedModule;
pub(crate) use hand_parser::ParseContext;

pub fn parse_input(input: &str) -> Result<ParsedModule> {
    let lexer = lexer::Token::lexer(input).spanned();

    match hand_parser::parse(
        &mut WhitespaceTrackingLexer {
            lexer,
            peeked: VecDeque::with_capacity(2),
            previous_was_newline: false,
        },
        ParseContext { top_level: true },
    ) {
        Err(error) => SyntaxError::new(pretty_print(input, error)).into(),
        Ok(module) => Ok(module),
    }
}

pub(crate) fn parse_input_as<'a, T: Parse<'a>>(input: &'a str, context: ParseContext) -> Result<T> {
    let lexer = lexer::Token::lexer(input).spanned();

    match T::parse(
        &mut WhitespaceTrackingLexer {
            lexer,
            peeked: VecDeque::with_capacity(2),
            previous_was_newline: false,
        },
        context,
    ) {
        Err(error) => SyntaxError::new(pretty_print(input, error)).into(),
        Ok(module) => Ok(module),
    }
}

#[cfg(test)]
mod test {
    use crate::parse_input;

    #[test]
    fn test_statement() {
        let str = "message += 'Expected SameValue(«' + assert._toString(actual) + '», «' + assert._toString(expected) + '») to be true';";

        parse_input(str).unwrap();
    }
}
