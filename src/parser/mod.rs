use crate::parser::ast::ParsedModule;

pub(crate) mod ast;
mod hand_parser;
mod lexer;

use logos::Logos;

pub(crate) fn parse_input<'a>(input: &'a str) -> ParsedModule<'a> {
    let mut lex = lexer::Token::lexer(input).peekable();

    hand_parser::parse(&mut lex).unwrap()
}
