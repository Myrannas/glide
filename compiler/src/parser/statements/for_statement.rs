use crate::parser::ast::{BinaryOperator, Expression, Reference, VarStatement};
use crate::parser::hand_parser::{parse_expression, Error, LexerImpl, Parse};
use crate::parser::hand_parser::{ParseContext, Result};
use crate::parser::lexer::Token;
use crate::parser::statements::statement::Statement;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ForStatement<'a> {
    For {
        expression: Option<Expression<'a>>,
        vars: Option<VarStatement<'a>>,
        condition: Option<Expression<'a>>,
        operation: Option<Expression<'a>>,
        block: Box<Statement<'a>>,
    },
    ForIn {
        allocate: bool,
        identifier: &'a str,
        expression: Expression<'a>,
        block: Box<Statement<'a>>,
    },
}

impl<'a> Parse<'a> for ForStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.expect(Token::For)?;

        input.expect(Token::OpenParen)?;

        let (vars, expression) = if input.lookahead_is(Token::Semicolon) {
            (None, None)
        } else if input.lookahead_is(Token::Var) {
            (Some(VarStatement::parse(input, context)?), None)
        } else {
            (None, Some(Expression::parse(input, context)?))
        };

        let result = match input.next() {
            Some((Token::Semicolon, ..)) => {
                let condition = if !input.lookahead_is(Token::Semicolon) {
                    Some(Expression::parse(input, context)?)
                } else {
                    None
                };
                input.expect(Token::Semicolon)?;

                let operation = if !input.lookahead_is(Token::CloseParen) {
                    Some(Expression::parse(input, context)?)
                } else {
                    None
                };
                input.expect(Token::CloseParen)?;

                let block = Statement::parse(input, context)?;

                ForStatement::For {
                    vars,
                    expression,
                    condition,
                    operation,
                    block: Box::new(block),
                }
            }
            Some((Token::In, ..)) => {
                let expression = parse_expression(input, context)?;

                input.expect(Token::CloseParen)?;

                return if let Some(VarStatement { declarations }) = vars {
                    Ok(ForStatement::ForIn {
                        allocate: true,
                        identifier: declarations[0].identifier,
                        expression,
                        block: Box::new(Statement::parse(input, context)?),
                    })
                } else {
                    Err(Error::SyntaxError {
                        message: "Invalid for(... in ...) block",
                    })
                };
            }
            Some(other) => {
                if let Some(Expression::BinaryExpression {
                    operator: BinaryOperator::In,
                    left,
                    right,
                }) = expression
                {
                    if let Expression::Reference(Reference::Id(id)) = *left {
                        return Ok(ForStatement::ForIn {
                            identifier: id,
                            allocate: false,
                            expression: *right,
                            block: Box::new(Statement::parse(input, context)?),
                        });
                    }
                }

                return input.expected(&[Token::Semicolon, Token::In], other);
            }
            _ => unreachable!(),
        };

        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use crate::parser::hand_parser::ParseContext;
    use crate::parser::parse_input_as;
    use crate::parser::statements::for_statement::ForStatement;
    use assert_matches::assert_matches;

    #[test]
    fn parses_for_statement() {
        let result =
            parse_input_as::<ForStatement>("for (var i = 0; i < 100; i++) {  }", ParseContext::TOP);

        assert_matches!(result, Ok(ForStatement::For { .. }));
    }

    #[test]
    fn parses_for_statement_with_peq() {
        let result = parse_input_as::<ForStatement>(
            "for(var index=0; index<10; index+=1) {}",
            ParseContext::TOP,
        );

        assert_matches!(result, Ok(ForStatement::For { .. }));
    }

    #[test]
    fn parses_for_in_statement() {
        let result = parse_input_as::<ForStatement>("for (var i in test) {  }", ParseContext::TOP);

        assert_matches!(result, Ok(ForStatement::ForIn { .. }));
    }
}
