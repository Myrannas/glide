use crate::parser::ast::{
    BlockStatement, Expression, IfStatement, ReturnStatement, ThrowStatement, TryStatement,
    VarStatement,
};
use crate::parser::hand_parser::{parse_expression, Error, LexerImpl, Parse};
use crate::parser::hand_parser::{ParseContext, Result};
use crate::parser::lexer::Token;
use crate::parser::statements::decl_statement::DeclStatement;
use crate::parser::statements::do_while_statement::DoWhileStatement;
use crate::parser::statements::for_statement::ForStatement;
use crate::parser::statements::while_statement::WhileStatement;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement<'a> {
    Label(&'a str, Box<Statement<'a>>),
    Block(BlockStatement<'a>),
    If(IfStatement<'a>),
    Return(ReturnStatement<'a>),
    While(WhileStatement<'a>),
    DoWhile(DoWhileStatement<'a>),
    Var(VarStatement<'a>),
    Expression(Expression<'a>),
    Try(TryStatement<'a>),
    For(ForStatement<'a>),
    Break(Option<&'a str>),
    Continue(Option<&'a str>),
    ThrowStatement(ThrowStatement<'a>),
    Empty,
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

                let break_id = if input.lookahead_is_id() {
                    Some(input.expect_id()?)
                } else {
                    None
                };

                Ok(Statement::Break(break_id))
            }
            Some((Token::Continue, ..)) => {
                input.next();

                let continue_id = if input.lookahead_is_id() {
                    Some(input.expect_id()?)
                } else {
                    None
                };

                Ok(Statement::Continue(continue_id))
            }
            Some((Token::Do, ..)) => {
                DoWhileStatement::parse(input, context).map(Statement::DoWhile)
            }
            Some((Token::Semicolon, ..)) => Ok(Statement::Empty),
            // Some((Token::Label(label), ..)) => {
            //     input.next();
            //
            //     let statement = Statement::parse(input, context)?;
            //
            //     Ok(Statement::Label(label, Box::new(statement)))
            // }
            Some((token, ..)) => {
                if token.is_identifier() {
                    if input.lookahead_n_is(2, Token::Colon) {
                        let label = token.identifier().unwrap();

                        input.next();
                        input.next();

                        return match DeclStatement::parse(input, context)? {
                            DeclStatement::Statement(statement) => {
                                Ok(Statement::Label(label, Box::new(statement)))
                            }
                            _ => Err(Error::SyntaxError {
                                message: "Invalid statement",
                            }),
                        };
                    }
                }

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
