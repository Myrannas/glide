use crate::parser::ast::ParsedModule;

pub(crate) mod ast;
mod hand_parser;
mod lexer;

use crate::parser::hand_parser::pretty_print;
use anyhow::{Context, Error, Result};
use logos::Logos;

pub fn parse_input(input: &str) -> Result<ParsedModule> {
    let mut lex = lexer::Token::lexer(input).spanned().peekable();

    match hand_parser::parse(&mut lex) {
        Err(error) => Err(Error::msg(pretty_print(input, error))),
        Ok(module) => Ok(module),
    }
}
