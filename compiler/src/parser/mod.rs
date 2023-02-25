pub(crate) mod ast;
mod hand_parser;
mod lexer;
mod strings;

use crate::parser::hand_parser::{pretty_print, WhitespaceTrackingLexer};
use crate::result::{Result, SyntaxError};
use logos::Logos;

pub use ast::ParsedModule;

pub fn parse_input(input: &str) -> Result<ParsedModule> {
    let lexer = lexer::Token::lexer(input).spanned();

    match hand_parser::parse(&mut WhitespaceTrackingLexer {
        lexer,
        peeked: None,
        previous_was_newline: false,
    }) {
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
