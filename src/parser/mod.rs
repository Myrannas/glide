use crate::parser::ast::ParsedModule;

pub(crate) mod ast;
mod hand_parser;
mod lexer;

use crate::parser::hand_parser::pretty_print;
use logos::Logos;

pub(crate) fn parse_input(input: &str) -> ParsedModule {
    let mut lex = lexer::Token::lexer(input).spanned().peekable();

    match hand_parser::parse(&mut lex) {
        Err(error) => panic!("{}", pretty_print(input, error)),
        Ok(module) => module,
    }
}
