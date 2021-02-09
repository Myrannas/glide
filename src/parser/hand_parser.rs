use super::ast::{Expression, Reference};
use crate::parser::ast::{
    FunctionStatement, IfStatement, ParsedModule, Return, Statement, VarStatement, WhileStatement,
};
use crate::parser::lexer::Token;
use std::iter::Peekable;

trait Parse<'a>
where
    Self: Sized,
{
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, Self>;
}

impl<'a> Parse<'a> for Expression<'a> {
    fn parse(input: &mut impl LexerUtils<'a, Item = Token<'a>>) -> Result<'a, Self> {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Error<'a> {
    Expected {
        expected: Vec<Token<'a>>,
        got: Token<'a>,
    },
    EndOfFile,
}
type Result<'a, T> = std::result::Result<T, Error<'a>>;

pub(crate) trait LexerUtils<'a>: Iterator<Item = Token<'a>> {
    fn expect(&mut self, token: Token<'a>) -> Result<'a, ()>;
    fn expect_id(&mut self) -> Result<'a, &'a str>;
    fn lookahead_fn<T>(&mut self, transform: impl Fn(Option<&Token<'a>>) -> T) -> T;
    fn lookahead(&mut self) -> Option<Token<'a>>;
    fn lookahead_is(&mut self, transform: Token<'a>) -> bool;
    fn consume_if(&mut self, transform: Token<'a>) -> bool;
}

impl<'a, T> LexerUtils<'a> for Peekable<T>
where
    T: Iterator<Item = Token<'a>> + Sized,
{
    fn expect(&mut self, token: Token<'a>) -> Result<'a, ()> {
        let next = self.next();

        if next == Some(token) {
            Ok(())
        } else {
            Err(Error::Expected {
                expected: vec![token],
                got: next.unwrap_or(Token::EndOfFile),
            })
        }
    }

    fn expect_id(&mut self) -> Result<'a, &'a str> {
        match self.next() {
            Some(Token::Id(id)) => Ok(id),
            other => Err(Error::Expected {
                expected: vec![Token::Id("")],
                got: other.unwrap_or(Token::EndOfFile),
            }),
        }
    }

    fn consume_if(&mut self, transform: Token<'a>) -> bool {
        if self.lookahead_is(transform) {
            self.next();
            true
        } else {
            false
        }
    }

    fn lookahead_fn<X>(&mut self, transform: impl Fn(Option<&Token<'a>>) -> X) -> X {
        transform(self.peek())
    }

    fn lookahead(&mut self) -> Option<Token<'a>> {
        self.peek().cloned()
    }

    fn lookahead_is(&mut self, transform: Token<'a>) -> bool {
        if let Some(v) = self.peek() {
            if *v == transform {
                return true;
            }
        }

        false
    }
}

fn parse_object_literal<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let mut attributes = Vec::new();

    while !input.consume_if(Token::CloseBrace) {
        match input.next() {
            Some(Token::Id(id)) => {
                input.expect(Token::Colon)?;
                let expression = parse_expression(input)?;

                attributes.push((id, expression))
            }
            _ => panic!("Ohnoes"),
        }
    }
    Ok(Expression::ObjectLiteral { attributes })
}

fn parse_value<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    if input.lookahead_is(Token::OpenParen) {
        return parse_group(input);
    }

    match input.next() {
        Some(Token::Id(identifier)) => Ok(Expression::Reference(Reference::Id(identifier))),
        Some(Token::Float(value)) => Ok(Expression::Float(value)),
        Some(Token::String(value)) => Ok(Expression::String(value)),
        Some(Token::Boolean(value)) => Ok(Expression::Boolean(value)),
        Some(Token::OpenBrace) => parse_object_literal(input),
        Some(other) => Err(Error::Expected {
            expected: vec![Token::Id("")],
            got: other,
        }),
        _ => panic!(""),
    }
}

fn parse_accessor<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let expression = parse_value(input)?;

    let null_safe = match input.lookahead() {
        Some(Token::Dot) => false,
        Some(Token::NullSafe) => true,
        _ => return Ok(expression),
    };

    input.next();

    let accessor = input.expect_id()?;

    Ok(Expression::Reference(Reference::Accessor {
        expression: Box::new(expression),
        accessor,
        null_safe,
    }))
}

fn parse_call<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let expression = parse_accessor(input)?;

    if input.consume_if(Token::OpenParen) {
        let mut parameters = Vec::new();

        while !input.consume_if(Token::CloseParen) {
            parameters.push(parse_expression(input)?);

            if !input.lookahead_is(Token::Comma) {
                input.expect(Token::CloseParen)?;
                break;
            }
        }

        Ok(Expression::Call {
            expression: Box::new(expression),
            parameters,
        })
    } else {
        Ok(expression)
    }
}

fn parse_commutative<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let mut left = parse_call(input)?;

    loop {
        let operator = match input.lookahead() {
            Some(Token::Multiply) => Expression::Mul,
            Some(Token::Divide) => Expression::Div,
            Some(Token::Modulus) => Expression::Mod,
            _ => return Ok(left),
        };
        input.next();
        left = operator(Box::new(left), Box::new(parse_commutative(input)?));
    }
}

fn parse_associative<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let mut left = parse_commutative(input)?;

    loop {
        let operator = match input.lookahead() {
            Some(Token::Add) => Expression::Add,
            Some(Token::Sub) => Expression::Sub,
            _ => return Ok(left),
        };
        input.next();
        left = operator(Box::new(left), Box::new(parse_commutative(input)?));
    }
}

fn parse_comparison<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let mut left = parse_associative(input)?;

    loop {
        let operator = match input.lookahead() {
            Some(Token::GreaterThan) => Expression::GreaterThan,
            Some(Token::GreaterThanEqual) => Expression::GreaterThanEqual,
            Some(Token::LessThan) => Expression::LessThan,
            Some(Token::LessThanEqual) => Expression::LessThanEqual,
            Some(Token::EqualTo) => Expression::EqualTo,
            Some(Token::NotEqualTo) => Expression::NotEqualTo,
            Some(Token::StrictEqualTo) => Expression::StrictEqualTo,
            Some(Token::NotStrictEqualTo) => Expression::NotStrictEqualTo,
            _ => return Ok(left),
        };

        input.next();

        left = operator(Box::new(left), Box::new(parse_commutative(input)?))
    }
}

fn parse_assignment<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    match parse_comparison(input)? {
        Expression::Reference(reference) => {
            if input.consume_if(Token::Assign) {
                Ok(Expression::Assign {
                    assign_to: reference,
                    expression: Box::new(parse_expression(input)?),
                })
            } else {
                Ok(Expression::Reference(reference))
            }
        }
        other => Ok(other),
    }
}

fn parse_expression<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    parse_assignment(input)
}

impl<'a> Parse<'a> for IfStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a, Item = Token<'a>>) -> Result<'a, Self> {
        input.expect(Token::If)?;

        let condition = parse_group(input)?;
        let true_block = parse_block(input)?;

        let false_block = if input.consume_if(Token::Else) {
            Some(parse_block(input)?)
        } else {
            None
        };

        Ok(IfStatement {
            condition,
            true_block,
            false_block,
        })
    }
}

impl<'a> Parse<'a> for WhileStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, Self> {
        input.expect(Token::While)?;

        let condition = parse_group(input)?;
        let loop_block = parse_block(input)?;

        Ok(WhileStatement {
            condition,
            loop_block,
        })
    }
}

impl<'a> Parse<'a> for VarStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, VarStatement<'a>> {
        input.expect(Token::Var)?;

        let identifier = input.expect_id()?;
        input.expect(Token::Assign)?;
        let expression = parse_expression(input)?;

        Ok(VarStatement {
            identifier,
            expression,
        })
    }
}

impl<'a> Parse<'a> for Statement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, Statement<'a>> {
        match input.lookahead() {
            Some(Token::Function) => FunctionStatement::parse(input).map(Statement::Function),
            Some(Token::If) => IfStatement::parse(input).map(Statement::IfStatement),
            Some(Token::Return) => Return::parse(input).map(Statement::Return),
            Some(Token::While) => WhileStatement::parse(input).map(Statement::While),
            Some(Token::Var) => VarStatement::parse(input).map(Statement::Var),
            Some(_) => Ok(Statement::Expression(parse_expression(input)?)),
            None => Err(Error::EndOfFile),
        }
    }
}

fn parse_group<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    input.expect(Token::OpenParen)?;

    let expr = parse_expression(input)?;

    input.expect(Token::CloseParen)?;

    Ok(expr)
}

fn parse_block<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Vec<Statement<'a>>> {
    input.expect(Token::OpenBrace)?;

    let mut statements = Vec::new();
    while !input.lookahead_is(Token::CloseBrace) {
        statements.push(Statement::parse(input)?);

        if input.lookahead_is(Token::Semicolon) {
            input.next();
        }
    }

    input.expect(Token::CloseBrace)?;
    Ok(statements)
}

impl<'a> Parse<'a> for Return<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, Return<'a>> {
        input.expect(Token::Return)?;

        let expression = parse_expression(input)?;

        Ok(Return { expression })
    }
}

impl<'a> Parse<'a> for FunctionStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, FunctionStatement<'a>> {
        input.expect(Token::Function)?;
        let identifier = input.expect_id()?;
        input.expect(Token::OpenParen)?;

        let mut arguments = Vec::new();

        while !input.lookahead_is(Token::CloseParen) {
            arguments.push(input.expect_id()?);

            if input.lookahead_is(Token::CloseParen) {
                break;
            } else {
                input.expect(Token::Comma)?;
            }
        }

        input.expect(Token::CloseParen)?;

        let statements = parse_block(input)?;

        Ok(FunctionStatement {
            identifier,
            arguments,
            statements,
        })
    }
}

pub(crate) fn parse<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, ParsedModule<'a>> {
    let mut statements = Vec::new();
    while input.lookahead().is_some() {
        statements.push(Statement::parse(input)?);

        if input.lookahead_is(Token::Semicolon) {
            input.next();
        }
    }

    Ok(ParsedModule { block: statements })
}

#[cfg(test)]
mod test {
    use super::super::lexer::Token;
    use super::*;
    use crate::parser::hand_parser::{parse_block, FunctionStatement, Statement};
    use logos::Logos;

    #[test]
    fn test_parse_function() {
        assert_eq!(
            FunctionStatement::parse(
                &mut Token::lexer("function hello(world, boom) { }").peekable()
            ),
            Ok(FunctionStatement {
                identifier: "hello",
                arguments: vec!["world", "boom"],
                statements: vec![]
            })
        );
    }

    #[test]
    fn test_parse_block() {
        assert_eq!(
            parse_block(&mut Token::lexer("{ console?.log('hi'); }").peekable()),
            Ok(vec![Statement::Expression(Expression::Call {
                expression: Box::new(Expression::Reference(Reference::Accessor {
                    expression: Box::new(Expression::Reference(Reference::Id("console"))),
                    accessor: "log",
                    null_safe: true
                })),
                parameters: vec![Expression::String("'hi'")]
            })])
        );
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            parse_expression(&mut Token::lexer("1.0 + Test").peekable()),
            Ok(Expression::Add(
                Box::new(Expression::Float(1.0)),
                Box::new(Expression::Reference(Reference::Id("Test")))
            ))
        );
    }

    #[test]
    fn test_if_else() {
        assert_eq!(
            IfStatement::parse(
                &mut Token::lexer("if (true) { return 1; } else { return 2; }").peekable()
            ),
            Ok(IfStatement {
                condition: Expression::Boolean(true),
                true_block: vec![Statement::Return(Return {
                    expression: Expression::Float(1.0)
                })],
                false_block: Some(vec![Statement::Return(Return {
                    expression: Expression::Float(2.0)
                })])
            })
        );
    }
}
