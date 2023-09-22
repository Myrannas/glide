use crate::parser::ast::Expression;
use crate::parser::hand_parser::{parse_group, LexerImpl, Parse};
use crate::parser::hand_parser::{ParseContext, Result};
use crate::parser::lexer::Token;
use crate::parser::statements::statement::Statement;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct DoWhileStatement<'a> {
    pub(crate) condition: Expression<'a>,
    pub(crate) loop_block: Box<Statement<'a>>,
}

impl<'a> Parse<'a> for DoWhileStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.expect(Token::Do)?;
        let loop_block = Statement::parse(input, context)?;

        input.expect(Token::While)?;
        let condition = parse_group(input, context)?;

        Ok(DoWhileStatement {
            condition,
            loop_block: Box::new(loop_block),
        })
    }
}
