pub(crate) mod ast;
mod hand_parser;
mod lexer;
mod strings;

use crate::parser::hand_parser::pretty_print;
use anyhow::{Context, Error, Result};
use logos::Logos;

pub fn parse_input<'a>(input: &str) -> StaticJsResult<ParsedModule> {
    let mut lex = lexer::Token::lexer(input).spanned().peekable();

    match hand_parser::parse(&mut lex) {
        Err(error) => SyntaxError::new(pretty_print(input, error)).into(),
        Ok(module) => Ok(module),
    }
}

use crate::result::{InternalError, JsResult, StaticJsResult, SyntaxError};
use crate::ExecutionError;
pub use ast::ParsedModule;