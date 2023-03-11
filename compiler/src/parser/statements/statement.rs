use crate::parser::ast::{
    BlockStatement, ClassStatement, ConstStatement, Expression, FunctionStatement, IfStatement,
    ReturnStatement, ThrowStatement, TryStatement, VarStatement, WhileStatement,
};
use crate::parser::hand_parser::{parse_expression, Error, LexerImpl, Parse};
use crate::parser::hand_parser::{ParseContext, Result};
use crate::parser::lexer::Token;
use crate::parser::statements::decl_statement::DeclStatement;
use crate::parser::statements::for_statement::ForStatement;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement<'a> {
    Block(BlockStatement<'a>),
    If(IfStatement<'a>),
    Return(ReturnStatement<'a>),
    While(WhileStatement<'a>),
    Var(VarStatement<'a>),
    Expression(Expression<'a>),
    Try(TryStatement<'a>),
    For(ForStatement<'a>),
    Break,
    Continue,
    ThrowStatement(ThrowStatement<'a>),
}

impl<'a> Parse<'a> for Statement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Statement<'a>> {
        match input.lookahead() {
            Some((Token::If, ..)) => IfStatement::parse(input, context).map(Statement::If),
            Some((Token::Return, ..)) => {
                if context.top_level {
                    return Err(Error::SyntaxError {
                        message: "Invalid context for return statement",
                    });
                }

                ReturnStatement::parse(input, context).map(Statement::Return)
            }
            Some((Token::While, ..)) => WhileStatement::parse(input, context).map(Statement::While),
            Some((Token::Var, ..)) => {
                let result = VarStatement::parse(input, context).map(Statement::Var)?;

                input.expect_end_of_statement()?;

                Ok(result)
            }
            Some((Token::Try, ..)) => TryStatement::parse(input, context).map(Statement::Try),
            Some((Token::Throw, ..)) => {
                ThrowStatement::parse(input, context).map(Statement::ThrowStatement)
            }
            Some((Token::OpenBrace, ..)) => {
                BlockStatement::parse(input, context).map(Statement::Block)
            }
            Some((Token::For, ..)) => ForStatement::parse(input, context).map(Statement::For),
            Some((Token::Break, ..)) => {
                input.next();

                Ok(Statement::Break)
            }
            Some((Token::Continue, ..)) => {
                input.next();

                Ok(Statement::Continue)
            }
            Some(_) => {
                let expression = parse_expression(input, context);

                input.expect_end_of_statement()?;

                Ok(Statement::Expression(expression?))
            }
            None => Err(Error::EndOfFile),
        }
    }
}

impl<'a> Into<BlockStatement<'a>> for Statement<'a> {
    fn into(self) -> BlockStatement<'a> {
        match self {
            Statement::Block(block) => block,
            other => BlockStatement {
                statements: vec![DeclStatement::Statement(other)],
            },
        }
    }
}
