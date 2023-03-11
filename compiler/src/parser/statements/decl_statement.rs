use crate::parser::ast::{ClassStatement, ConstStatement, FunctionStatement};
use crate::parser::hand_parser::{Error, LexerImpl, Parse, ParseContext};
use crate::parser::lexer::Token;
use crate::parser::statements::statement::Statement;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum DeclStatement<'a> {
    Class(ClassStatement<'a>),
    Function(FunctionStatement<'a>),
    Statement(Statement<'a>),
    Const(ConstStatement<'a>),
}

impl<'a> Parse<'a> for DeclStatement<'a> {
    fn parse(
        input: &mut LexerImpl<'a>,
        context: ParseContext,
    ) -> crate::parser::hand_parser::Result<'a, Self> {
        match input.lookahead() {
            Some((Token::Function, ..)) => {
                FunctionStatement::parse(input, context).map(DeclStatement::Function)
            }
            Some((Token::Class, ..)) => {
                ClassStatement::parse(input, context).map(DeclStatement::Class)
            }
            Some((Token::Const, ..)) => {
                ConstStatement::parse(input, context).map(DeclStatement::Const)
            }
            Some(_) => Statement::parse(input, context).map(DeclStatement::Statement),
            None => Err(Error::EndOfFile),
        }
    }
}
