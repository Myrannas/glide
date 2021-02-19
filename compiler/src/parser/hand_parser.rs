use super::ast::{Expression, Reference};
use crate::parser::ast::{
    FunctionStatement, IfStatement, ParsedModule, ReturnStatement, Statement, TryStatement,
    VarStatement, WhileStatement,
};
use crate::parser::hand_parser::Error::Expected;
use crate::parser::lexer::Token;
use logos::Span;
use std::cmp::min;
use std::iter::Peekable;
use std::ops::Range;

trait Parse<'a>
where
    Self: Sized,
{
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, Self>;
}

impl<'a> Parse<'a> for Expression<'a> {
    fn parse(_input: &mut impl LexerUtils<'a, Item = (Token<'a>, Span)>) -> Result<'a, Self> {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Error<'a> {
    Expected {
        expected: Vec<Token<'a>>,
        got: Token<'a>,
        location: Range<usize>,
    },
    EndOfFile,
}

pub(crate) fn pretty_print(input: &str, err: Error) -> String {
    match err {
        Error::EndOfFile => format!("End of file"),
        Error::Expected {
            expected,
            got,
            location,
        } => {
            let mut start = location.start as isize;
            let mut start_in_line = 0;
            let mut containing_line = "";
            let mut line_number = 0;
            for line in input.lines() {
                let in_line = start;
                start -= 1 + line.len() as isize;
                line_number += 1;

                if start <= 0 {
                    containing_line = line;
                    start_in_line = in_line;
                    break;
                }
            }

            let marker = format!(
                "{}{}",
                " ".repeat(start_in_line as usize),
                "^".repeat(min(location.len(), containing_line.len()))
            );

            format!(
                "Expected one of {:?} but got {:?} at ({}:{}) \n{}\n{}",
                expected,
                got,
                line_number,
                start_in_line + 1,
                containing_line,
                marker
            )
        }
    }
}

type Result<'a, T> = std::result::Result<T, Error<'a>>;

pub(crate) trait LexerUtils<'a>: Iterator<Item = (Token<'a>, Span)> {
    fn expect(&mut self, token: Token<'a>) -> Result<'a, ()>;
    fn expected<T>(&self, expected: Vec<Token<'a>>, got: Self::Item) -> Result<'a, T>;
    fn expect_id(&mut self) -> Result<'a, &'a str>;
    fn lookahead_fn<T>(&mut self, transform: impl Fn(Option<&Self::Item>) -> T) -> T;
    fn lookahead(&mut self) -> Option<Self::Item>;
    fn lookahead_is(&mut self, transform: Token<'a>) -> bool;
    fn consume_if(&mut self, transform: Token<'a>) -> bool;
}

impl<'a, T> LexerUtils<'a> for Peekable<T>
where
    T: Iterator<Item = (Token<'a>, Span)> + Sized,
{
    fn expect(&mut self, token: Token<'a>) -> Result<'a, ()> {
        let next = self.next();

        if let Some((next_token, ..)) = next {
            if next_token == token {
                return Ok(());
            }
        }

        self.expected(
            vec![token],
            next.unwrap_or((Token::EndOfFile, 0..0)), // todo: figure out EOF
        )
    }

    #[inline]
    fn expected<X>(
        &self,
        tokens: Vec<Token<'a>>,
        (token, span): (Token<'a>, Span),
    ) -> Result<'a, X> {
        Err(Expected {
            expected: tokens,
            got: token,
            location: span,
        })
    }

    fn expect_id(&mut self) -> Result<'a, &'a str> {
        match self.next() {
            Some((Token::Id(id), ..)) => Ok(id),
            other => self.expected(
                vec![Token::Id("")],
                other.unwrap_or((Token::EndOfFile, 0..0)),
            ),
        }
    }

    #[inline]
    fn lookahead_fn<X>(&mut self, transform: impl Fn(Option<&Self::Item>) -> X) -> X {
        let peeked = self.peek();
        transform(peeked)
    }

    #[inline]
    fn lookahead(&mut self) -> Option<Self::Item> {
        let peeked = self.peek();
        peeked.cloned()
    }

    fn lookahead_is(&mut self, transform: Token<'a>) -> bool {
        if let Some((token, ..)) = self.peek() {
            if *token == transform {
                return true;
            }
        }

        false
    }

    fn consume_if(&mut self, transform: Token<'a>) -> bool {
        if self.lookahead_is(transform) {
            self.next();
            true
        } else {
            false
        }
    }
}

fn parse_object_literal<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let mut attributes = Vec::new();

    while !input.consume_if(Token::CloseBrace) {
        match input.next() {
            Some((Token::Id(id), ..)) => {
                input.expect(Token::Colon)?;
                let expression = parse_expression(input)?;

                attributes.push((id, expression))
            }
            Some(other) => input.expected(vec![Token::Id("")], other)?,
            _ => panic!("None!"),
        }
    }
    Ok(Expression::ObjectLiteral { attributes })
}

fn parse_value<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    if input.lookahead_is(Token::OpenParen) {
        return parse_group(input);
    }

    match input.next() {
        Some((Token::Id(identifier), ..)) => Ok(Expression::Reference(Reference::Id(identifier))),
        Some((Token::Float(value), ..)) => Ok(Expression::Float(value)),
        Some((Token::String(value), ..)) => Ok(Expression::String(value)),
        Some((Token::Boolean(value), ..)) => Ok(Expression::Boolean(value)),
        Some((Token::OpenBrace, ..)) => parse_object_literal(input),
        Some((Token::Undefined, ..)) => Ok(Expression::Undefined),
        Some((Token::Null, ..)) => Ok(Expression::Null),
        Some(other) => input.expected(
            vec![
                Token::Id(""),
                Token::Float(0.0),
                Token::String(""),
                Token::Boolean(false),
                Token::OpenBrace,
            ],
            other,
        ),
        _ => panic!(""),
    }
}

fn parse_accessor<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let is_new = input.consume_if(Token::New);

    let mut expression = parse_value(input)?;

    loop {
        let null_safe = match input.lookahead() {
            Some((Token::Dot, ..)) => false,
            Some((Token::NullSafe, ..)) => true,
            _ => break,
        };

        input.next();

        let accessor = input.expect_id()?;

        expression = Expression::Reference(Reference::Accessor {
            expression: Box::new(expression),
            accessor,
            null_safe,
        })
    }

    if is_new {
        if input.consume_if(Token::OpenParen) {
            let mut parameters = Vec::new();

            while !input.consume_if(Token::CloseParen) {
                parameters.push(parse_expression(input)?);

                if !input.lookahead_is(Token::Comma) {
                    input.expect(Token::CloseParen)?;
                    break;
                }

                input.expect(Token::Comma)?;
            }

            Ok(Expression::NewWithArgs {
                target: Box::new(expression),
                parameters,
            })
        } else {
            Ok(Expression::New(Box::new(expression)))
        }
    } else {
        Ok(expression)
    }
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

            input.expect(Token::Comma)?;
        }

        Ok(Expression::Call {
            expression: Box::new(expression),
            parameters,
        })
    } else {
        Ok(expression)
    }
}

fn parse_binary_expression<'a>(
    input: &mut impl LexerUtils<'a>,
    left: Expression<'a>,
    matcher: impl Fn(Token) -> Option<fn(Box<Expression<'a>>, Box<Expression<'a>>) -> Expression<'a>>,
) -> Result<'a, Expression<'a>> {
    input
        .lookahead()
        .ok_or(Error::EndOfFile)
        .and_then(move |(token, ..)| match matcher(token) {
            Some(operator) => {
                input.next();

                Ok(operator(Box::new(left), Box::new(parse_expression(input)?)))
            }
            None => Ok(left),
        })
}

fn parse_unary<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let result = match input.lookahead() {
        Some((Token::LogicalNot, ..)) => {
            input.next();
            Expression::LogicalNot(Box::new(parse_expression(input)?))
        }
        Some((Token::TypeOf, ..)) => {
            input.next();
            Expression::TypeOf(Box::new(parse_expression(input)?))
        }
        Some((Token::Sub, ..)) => {
            input.next();
            Expression::Neg(Box::new(parse_expression(input)?))
        }
        _ => parse_call(input)?,
    };

    Ok(result)
}

fn parse_commutative<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let left = parse_unary(input)?;

    parse_binary_expression(input, left, |token| match token {
        Token::Multiply => Some(Expression::Mul),
        Token::Divide => Some(Expression::Div),
        Token::Modulus => Some(Expression::Mod),
        _ => None,
    })
}

fn parse_associative<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let left = parse_commutative(input)?;

    parse_binary_expression(input, left, |token| match token {
        Token::Add => Some(Expression::Add),
        Token::Sub => Some(Expression::Sub),
        _ => None,
    })
}

fn parse_comparison<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let left = parse_associative(input)?;

    parse_binary_expression(input, left, |token| match token {
        Token::GreaterThan => Some(Expression::GreaterThan),
        Token::GreaterThanEqual => Some(Expression::GreaterThanEqual),
        Token::LessThan => Some(Expression::LessThan),
        Token::LessThanEqual => Some(Expression::LessThanEqual),
        _ => None,
    })
}

fn parse_equality<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let left = parse_comparison(input)?;

    parse_binary_expression(input, left, |token| match token {
        Token::EqualTo => Some(Expression::EqualTo),
        Token::NotEqualTo => Some(Expression::NotEqualTo),
        Token::StrictEqualTo => Some(Expression::StrictEqualTo),
        Token::NotStrictEqualTo => Some(Expression::NotStrictEqualTo),
        _ => None,
    })
}

fn parse_logical_and<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let left = parse_equality(input)?;

    parse_binary_expression(input, left, |token| match token {
        Token::LogicalAnd => Some(Expression::LogicalAnd),
        _ => None,
    })
}

fn parse_logical_or<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    let left = parse_logical_and(input)?;

    parse_binary_expression(input, left, |token| match token {
        Token::LogicalOr => Some(Expression::LogicalOr),
        _ => None,
    })
}

fn parse_assignment<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    match parse_logical_or(input)? {
        Expression::Reference(reference) => match input.lookahead() {
            Some((Token::Assign, ..)) => {
                input.next();
                Ok(Expression::Assign {
                    assign_to: reference,
                    expression: Box::new(parse_expression(input)?),
                })
            }
            Some((Token::AddAssign, ..)) => {
                input.next();
                Ok(Expression::Assign {
                    assign_to: reference.clone(),
                    expression: Box::new(Expression::Add(
                        Box::new(Expression::Reference(reference)),
                        Box::new(parse_expression(input)?),
                    )),
                })
            }
            _ => Ok(Expression::Reference(reference)),
        },
        other => Ok(other),
    }
}

fn parse_function_expression<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    if input.consume_if(Token::Function) {
        let arguments = parse_args_list(input)?;
        let statements = parse_block(input)?;

        Ok(Expression::Function {
            name: None,
            arguments,
            statements,
        })
    } else {
        parse_assignment(input)
    }
}

fn parse_expression<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Expression<'a>> {
    parse_function_expression(input)
}

impl<'a> Parse<'a> for IfStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, Self> {
        input.expect(Token::If)?;

        let condition = parse_group(input)?;
        let true_block = parse_block(input)?;

        let false_block = if input.consume_if(Token::Else) {
            if input.lookahead_is(Token::If) {
                Some(vec![Statement::If(IfStatement::parse(input)?)])
            } else {
                Some(parse_block(input)?)
            }
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
            Some((Token::Function, ..)) => FunctionStatement::parse(input).map(Statement::Function),
            Some((Token::If, ..)) => IfStatement::parse(input).map(Statement::If),
            Some((Token::Return, ..)) => ReturnStatement::parse(input).map(Statement::Return),
            Some((Token::While, ..)) => WhileStatement::parse(input).map(Statement::While),
            Some((Token::Var, ..)) => VarStatement::parse(input).map(Statement::Var),
            Some((Token::Try, ..)) => TryStatement::parse(input).map(Statement::Try),
            Some(_) => Ok(Statement::Expression(parse_expression(input)?)),
            None => Err(Error::EndOfFile),
        }
    }
}

fn parse_args_list<'a>(input: &mut impl LexerUtils<'a>) -> Result<'a, Vec<&'a str>> {
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

    Ok(arguments)
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

impl<'a> Parse<'a> for TryStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, Self> {
        input.expect(Token::Try)?;

        let try_block = parse_block(input)?;

        let (catch_binding, catch_block) = if input.consume_if(Token::Catch) {
            let binding = if input.consume_if(Token::OpenParen) {
                let id = input.expect_id()?;
                input.expect(Token::CloseParen)?;
                Some(id)
            } else {
                None
            };

            let block = parse_block(input)?;

            (binding, Some(block))
        } else {
            (None, None)
        };

        let finally_block = if input.consume_if(Token::Finally) {
            Some(parse_block(input)?)
        } else {
            None
        };

        Ok(TryStatement {
            try_block,
            catch_binding,
            catch_block,
            finally_block,
        })
    }
}

impl<'a> Parse<'a> for ReturnStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, ReturnStatement<'a>> {
        input.expect(Token::Return)?;

        if input.lookahead_is(Token::Semicolon) {
            Ok(ReturnStatement { expression: None })
        } else {
            let expression = parse_expression(input)?;

            Ok(ReturnStatement {
                expression: Some(expression),
            })
        }
    }
}

impl<'a> Parse<'a> for FunctionStatement<'a> {
    fn parse(input: &mut impl LexerUtils<'a>) -> Result<'a, FunctionStatement<'a>> {
        input.expect(Token::Function)?;
        let identifier = input.expect_id()?;
        let arguments = parse_args_list(input)?;
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
                &mut Token::lexer("function hello(world, boom) { }")
                    .spanned()
                    .peekable()
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
            parse_block(&mut Token::lexer("{ console?.log('hi'); }").spanned().peekable()),
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
            parse_expression(&mut Token::lexer("1.0 + Test").spanned().peekable()),
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
                &mut Token::lexer("if (true) { return 1; } else { return; }")
                    .spanned()
                    .peekable()
            ),
            Ok(IfStatement {
                condition: Expression::Boolean(true),
                true_block: vec![Statement::Return(ReturnStatement {
                    expression: Some(Expression::Float(1.0))
                })],
                false_block: Some(vec![Statement::Return(ReturnStatement {
                    expression: Some(Expression::Float(2.0))
                })])
            })
        );
    }
}
