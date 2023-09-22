use crate::parser::ast::Expression;
use crate::parser::hand_parser::{parse_group, LexerImpl, Parse};
use crate::parser::hand_parser::{ParseContext, Result};
use crate::parser::lexer::Token;
use crate::parser::statements::statement::Statement;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct WhileStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) loop_block: Box<Statement<'a>>,
}

impl<'a> Parse<'a> for WhileStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.expect(Token::While)?;

        let condition = parse_group(input, context)?;
        let loop_block = Statement::parse(input, context)?;

        Ok(WhileStatement {
            condition,
            loop_block: Box::new(loop_block),
        })
    }
}
