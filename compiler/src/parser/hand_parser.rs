use super::ast::{Expression, Reference};
use crate::parser::ast::{
    BinaryOperator, BlockStatement, ClassMember, ClassStatement, ConstStatement, Field,
    FunctionStatement, IfStatement, ParsedModule, ReturnStatement, ThrowStatement, TryStatement,
    UnaryOperator, VarDeclaration, VarStatement, WhileStatement,
};
use crate::parser::hand_parser::Error::Expected;
use crate::parser::lexer::Token;
use crate::parser::statements::decl_statement::DeclStatement;
use crate::parser::statements::for_statement::ForStatement;
use crate::parser::statements::statement::Statement;
use crate::parser::strings::parse_string;
use logos::{Source, Span, SpannedIter};
use std::backtrace::Backtrace;
use std::cmp::min;
use std::fmt::{Display, Formatter};
use std::ops::Range;

pub(crate) struct WhitespaceTrackingLexer<'a> {
    pub(crate) lexer: SpannedIter<'a, Token<'a>>,
    pub(crate) peeked: Option<Option<(Token<'a>, Span)>>,
    pub(crate) previous_was_newline: bool,
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct ParseContext {
    pub(crate) top_level: bool,
}

impl ParseContext {
    pub(crate) const TOP: ParseContext = ParseContext { top_level: true };
}

pub(crate) trait Parse<'a>
where
    Self: Sized,
{
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self>;
}

impl<'a> Parse<'a> for Expression<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        parse_expression(input, context)
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, PartialEq)]
pub(crate) enum Error<'a> {
    Expected {
        expected: Vec<Token<'a>>,
        got: Token<'a>,
        location: Range<usize>,
    },
    SyntaxError {
        message: &'static str,
    },
    InvalidUnicodeEscape,
    EndOfFile,
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl<'a> std::error::Error for Error<'a> {}

pub(crate) fn pretty_print(input: &str, err: Error) -> String {
    match err {
        Error::EndOfFile => "End of file".to_string(),
        Error::SyntaxError { message } => message.to_owned(),
        Error::InvalidUnicodeEscape => "Invalid Unicode escape sequence".to_string(),
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

pub(crate) type Result<'a, T> = std::result::Result<T, Error<'a>>;
pub(crate) type LexerImpl<'a> = WhitespaceTrackingLexer<'a>;

impl<'a> WhitespaceTrackingLexer<'a> {
    pub(crate) fn peek(&mut self) -> Option<&(Token<'a>, Span)> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next())
        }

        self.peeked.as_ref().unwrap().as_ref()
    }

    pub(crate) fn next(&mut self) -> Option<(Token<'a>, Span)> {
        if let Some(peeked) = self.peeked.take() {
            // println!("{:?}", peeked);

            return peeked;
        }

        self.previous_was_newline = false;
        while let Some(next) = self.lexer.next() {
            match next {
                (Token::NewLine, ..) => {
                    self.previous_was_newline = true;
                }
                (Token::Comment, ..) => {}
                (Token::BlockComment(comment), ..) => {
                    if comment.contains('\n') {
                        self.previous_was_newline = true;
                    }

                    if comment.contains('\r') {
                        self.previous_was_newline = true;
                    }

                    if comment.contains('\u{2029}') {
                        self.previous_was_newline = true;
                    }

                    if comment.contains('\u{2028}') {
                        self.previous_was_newline = true;
                    }
                }
                other => {
                    // println!("{:?}", other);

                    let bt = Backtrace::capture();

                    // do_some_work();

                    // println!("{}", bt);

                    return Some(other);
                }
            }
        }

        None
    }

    pub(crate) fn expect(&mut self, token: Token<'a>) -> Result<'a, ()> {
        let next = self.next();

        if let Some((next_token, ..)) = next {
            if next_token == token {
                return Ok(());
            }
        }

        self.expected(
            &[token],
            next.unwrap_or((Token::EndOfFile, 0..0)), // todo: figure out EOF
        )
    }

    pub(crate) fn expect_end_of_statement(&mut self) -> Result<'a, ()> {
        let other = match self.peek() {
            Some((Token::Semicolon, ..)) => {
                self.next();
                return Ok(());
            }
            Some((Token::CloseBrace, ..)) => {
                return Ok(());
            }
            None => return Ok(()),
            other => other.unwrap_or(&(Token::EndOfFile, 0..0)).clone(),
        };

        if self.previous_was_newline {
            Ok(())
        } else {
            self.expected(
                &[Token::Semicolon, Token::CloseBrace, Token::NewLine],
                other,
            )
        }
    }

    pub(crate) fn expect_one_of(&mut self, tokens: &[Token<'a>]) -> Result<'a, ()> {
        let next = self.next().unwrap_or((Token::EndOfFile, 0..0));

        if tokens.iter().any(|t| *t == next.0) {
            return Ok(());
        }

        self.expected(
            tokens, next, // todo: figure out EOF
        )
    }

    #[inline]
    pub(crate) fn expected<X>(
        &self,
        tokens: &[Token<'a>],
        (token, span): (Token<'a>, Span),
    ) -> Result<'a, X> {
        Err(Expected {
            expected: tokens.to_vec(),
            got: token,
            location: span,
        })
    }

    #[inline]
    pub(crate) fn expect_id(&mut self) -> Result<'a, &'a str> {
        match self.next() {
            Some((Token::Id(id), ..)) => Ok(id),
            other => self.expected(&[Token::Id("")], other.unwrap_or((Token::EndOfFile, 0..0))),
        }
    }

    #[inline]
    pub(crate) fn lookahead_fn<X>(&mut self, transform: impl Fn(Option<&(Token, Span)>) -> X) -> X {
        let peeked = self.peek();
        transform(peeked)
    }

    #[inline]
    pub(crate) fn lookahead(&mut self) -> Option<(Token<'a>, Span)> {
        let peeked = self.peek();
        peeked.cloned()
    }

    #[inline]
    pub(crate) fn lookahead_is(&mut self, token: Token<'a>) -> bool {
        if let Some((next_token, ..)) = self.peek() {
            if *next_token == token {
                return true;
            }
        }

        false
    }

    #[inline]
    pub(crate) fn consume_if(&mut self, transform: Token<'a>) -> bool {
        if self.lookahead_is(transform) {
            self.next();
            true
        } else {
            false
        }
    }
}

fn parse_object_literal<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    let mut attributes = Vec::new();

    while !input.consume_if(Token::CloseBrace) {
        match input.next() {
            Some((Token::Id(id), ..)) => {
                input.expect(Token::Colon)?;
                let expression = parse_expression(input, context)?;

                attributes.push((id.to_owned(), expression))
            }
            Some((Token::Float(value), ..)) => {
                input.expect(Token::Colon)?;
                let expression = parse_expression(input, context)?;
                attributes.push((value.to_string(), expression))
            }
            Some(other) => input.expected(&[Token::Id("")], other)?,
            _ => panic!("None!"),
        }

        if !input.consume_if(Token::Comma) {
            input.expect(Token::CloseBrace)?;
            break;
        }
    }
    Ok(Expression::ObjectLiteral { attributes })
}

fn parse_array_literal<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    let mut attributes = Vec::new();

    while !input.consume_if(Token::CloseBracket) {
        if input.consume_if(Token::Comma) {
            attributes.push(Expression::Undefined);
            continue;
        }

        attributes.push(parse_expression(input, context)?);

        if !input.consume_if(Token::Comma) {
            input.expect(Token::CloseBracket)?;
            break;
        }
    }
    Ok(Expression::ArrayLiteral { attributes })
}

fn parse_value<'a>(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Expression<'a>> {
    if input.lookahead_is(Token::OpenParen) {
        return parse_group(input, context);
    }

    match input.next() {
        Some((Token::Id(identifier), ..)) => Ok(Expression::Reference(Reference::Id(identifier))),
        Some((Token::This, ..)) => Ok(Expression::Reference(Reference::This)),
        Some((Token::Float(value), ..)) => Ok(Expression::Float(value)),
        Some((Token::String(value), ..)) => {
            let decoded_value = parse_string(value.slice(1..(value.len() - 1)).unwrap())?;
            Ok(Expression::String(decoded_value))
        }
        Some((Token::TemplateString(value), ..)) => {
            let decoded_value = value.slice(1..(value.len() - 1)).unwrap();
            Ok(Expression::String(decoded_value.to_owned()))
        }
        Some((Token::Boolean(value), ..)) => Ok(Expression::Boolean(value)),
        Some((Token::OpenBrace, ..)) => parse_object_literal(input, context),
        Some((Token::OpenBracket, ..)) => parse_array_literal(input, context),
        Some((Token::Null, ..)) => Ok(Expression::Null),
        Some((Token::Void, ..)) => {
            parse_expression(input, context)?;

            Ok(Expression::Undefined)
        }
        Some((Token::Function, ..)) => {
            let arguments = parse_args_list(input)?;
            let statements = BlockStatement::parse(input, ParseContext { top_level: false })?;

            Ok(Expression::Function {
                name: None,
                arguments,
                statements,
            })
        }
        Some(other) => input.expected(
            &[
                Token::Id(""),
                Token::Float(0.0),
                Token::String(""),
                Token::Boolean(false),
                Token::OpenBrace,
            ],
            other,
        ),
        _ => Err(Error::EndOfFile),
    }
}

fn parse_accessor<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    let mut is_new = input.consume_if(Token::New);

    let mut expression = parse_value(input, context)?;

    loop {
        let (null_safe, computed, call) = match input.lookahead() {
            Some((Token::Dot, ..)) => (false, false, false),
            // Some((Token::NullSafe, ..)) => (true, false, false),
            Some((Token::OpenBracket, ..)) => (false, true, false),
            Some((Token::OpenParen, ..)) => (false, false, true),
            other => {
                expression = if is_new {
                    Expression::NewWithArgs {
                        target: Box::new(expression),
                        parameters: vec![],
                    }
                } else {
                    expression
                };

                break;
            }
        };

        input.next();

        expression = if call {
            let mut parameters = Vec::new();

            while !input.consume_if(Token::CloseParen) {
                parameters.push(parse_expression(input, context)?);

                if !input.lookahead_is(Token::Comma) {
                    input.expect(Token::CloseParen)?;
                    break;
                }

                input.expect(Token::Comma)?;
            }

            if is_new {
                is_new = false;

                Expression::NewWithArgs {
                    target: Box::new(expression),
                    parameters,
                }
            } else {
                Expression::Call {
                    expression: Box::new(expression),
                    parameters,
                }
            }
        } else if computed {
            let accessor = parse_expression(input, context)?;

            let expression = Expression::Reference(Reference::ComputedAccessor {
                expression: Box::new(expression),
                accessor: Box::new(accessor),
                null_safe,
            });

            input.expect(Token::CloseBracket)?;

            expression
        } else {
            let accessor = input.expect_id()?;

            Expression::Reference(Reference::Accessor {
                expression: Box::new(expression),
                accessor,
                null_safe,
            })
        };
    }

    Ok(expression)
}

fn parse_binary_expression<'a>(
    input: &mut LexerImpl<'a>,
    next: impl Fn(&mut LexerImpl<'a>, ParseContext) -> Result<'a, Expression<'a>>,
    matcher: impl Fn(Token) -> Option<BinaryOperator>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    let mut left = next(input, context)?;

    loop {
        match input.lookahead() {
            Some((token, _span)) => match matcher(token) {
                Some(operator) => {
                    input.next();

                    left = Expression::BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(next(input, context)?),
                        operator,
                    }
                }
                None => return Ok(left),
            },
            None => return Ok(left),
        }
    }
}

fn parse_unary<'a>(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Expression<'a>> {
    let operator = match input.lookahead() {
        Some((Token::LogicalNot, ..)) => Some(UnaryOperator::LogicalNot),
        Some((Token::TypeOf, ..)) => Some(UnaryOperator::TypeOf),
        Some((Token::Delete, ..)) => Some(UnaryOperator::Delete),
        Some((Token::Sub, ..)) => Some(UnaryOperator::Sub),
        Some((Token::Add, ..)) => Some(UnaryOperator::Add),
        Some((Token::Inc, ..)) => Some(UnaryOperator::PrefixInc),
        Some((Token::Dec, ..)) => Some(UnaryOperator::PrefixDec),
        _ => None,
    };

    let result = if let Some(operator) = operator {
        input.next();

        Expression::UnaryExpression {
            value: Box::new(parse_accessor(input, context)?),
            operator,
        }
    } else {
        parse_accessor(input, context)?
    };

    let result = match input.lookahead() {
        Some((Token::Inc, ..)) => {
            if input.previous_was_newline {
                return Err(Error::SyntaxError {
                    message: "Invalid symbol before ++ symbol",
                });
            }

            input.next();

            if let Expression::Reference(reference) = result {
                Expression::Inc {
                    reference,
                    pre: false,
                }
            } else {
                return Err(Error::SyntaxError {
                    message: "Invalid left-hand side expression in postfix operation",
                });
            }
        }
        Some((Token::Dec, ..)) => {
            input.next();

            if let Expression::Reference(reference) = result {
                Expression::Dec {
                    reference,
                    pre: false,
                }
            } else {
                return Err(Error::SyntaxError {
                    message: "Invalid left-hand side expression in postfix operation",
                });
            }
        }
        _ => result,
    };

    Ok(result)
}

fn parse_exponential<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_binary_expression(
        input,
        parse_unary,
        |token| match token {
            Token::Exponential => Some(BinaryOperator::Exponential),
            _ => None,
        },
        context,
    )
}

fn parse_commutative<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_binary_expression(
        input,
        parse_exponential,
        |token| match token {
            Token::Multiply => Some(BinaryOperator::Mul),
            Token::Divide => Some(BinaryOperator::Div),
            Token::Modulus => Some(BinaryOperator::Mod),
            _ => None,
        },
        context,
    )
}

fn flatten_adds<'a>(expression: Expression<'a>, expressions: &mut Vec<Expression<'a>>) {
    if let Expression::BinaryExpression {
        operator: BinaryOperator::Add,
        left,
        right,
    } = expression
    {
        flatten_adds(*left, expressions);
        flatten_adds(*right, expressions);
    } else {
        expressions.push(expression)
    };
}

fn parse_associative<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    let result = parse_binary_expression(
        input,
        parse_commutative,
        |token| match token {
            Token::Add => Some(BinaryOperator::Add),
            Token::Sub => Some(BinaryOperator::Sub),
            _ => None,
        },
        context,
    )?;

    if let Expression::BinaryExpression {
        operator: BinaryOperator::Add,
        left,
        right,
    } = result
    {
        let mut expressions = Vec::new();
        flatten_adds(*left, &mut expressions);
        flatten_adds(*right, &mut expressions);

        Ok(Expression::Add { expressions })
    } else {
        Ok(result)
    }
}

fn parse_shifts<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_binary_expression(
        input,
        parse_associative,
        |token| match token {
            Token::LeftShift => Some(BinaryOperator::LeftShift),
            Token::RightShift => Some(BinaryOperator::RightShift),
            Token::UnsignedRightShift => Some(BinaryOperator::RightShiftUnsigned),
            _ => None,
        },
        context,
    )
}

fn parse_comparison<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_binary_expression(
        input,
        parse_shifts,
        |token| match token {
            Token::GreaterThan => Some(BinaryOperator::GreaterThan),
            Token::GreaterThanEqual => Some(BinaryOperator::GreaterThanEqual),
            Token::LessThan => Some(BinaryOperator::LessThan),
            Token::LessThanEqual => Some(BinaryOperator::LessThanEqual),
            Token::InstanceOf => Some(BinaryOperator::InstanceOf),
            Token::In => Some(BinaryOperator::In),
            _ => None,
        },
        context,
    )
}

fn parse_equality<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_binary_expression(
        input,
        parse_comparison,
        |token| match token {
            Token::EqualTo => Some(BinaryOperator::EqualTo),
            Token::NotEqualTo => Some(BinaryOperator::NotEqualTo),
            Token::StrictEqualTo => Some(BinaryOperator::StrictEqualTo),
            Token::NotStrictEqualTo => Some(BinaryOperator::NotStrictEqualTo),
            _ => None,
        },
        context,
    )
}

fn parse_logical_and<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_binary_expression(
        input,
        parse_equality,
        |token| match token {
            Token::LogicalAnd => Some(BinaryOperator::LogicalAnd),
            _ => None,
        },
        context,
    )
}

fn parse_logical_or<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_binary_expression(
        input,
        parse_logical_and,
        |token| match token {
            Token::LogicalOr => Some(BinaryOperator::LogicalOr),
            _ => None,
        },
        context,
    )
}

fn parse_ternary<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    let condition = parse_logical_or(input, context)?;

    if input.consume_if(Token::QuestionMark) {
        let if_true = parse_assignment(input, context)?;
        input.expect(Token::Colon)?;
        let if_false = parse_assignment(input, context)?;

        Ok(Expression::ConditionalOperator {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        })
    } else {
        Ok(condition)
    }
}

fn as_op_assign<'a>(
    input: &mut LexerImpl<'a>,
    reference: Reference<'a>,
    context: ParseContext,
    operator: BinaryOperator,
) -> Result<'a, Expression<'a>> {
    Ok(Expression::Assign {
        assign_to: reference.clone(),
        expression: Box::new(Expression::BinaryExpression {
            left: Box::new(Expression::Reference(reference)),
            right: Box::new(parse_expression(input, context)?),
            operator,
        }),
    })
}

fn parse_assignment<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    match parse_ternary(input, context)? {
        Expression::Reference(reference) => match input.lookahead() {
            Some((Token::Assign, ..)) => {
                input.next();

                let expression = parse_expression(input, context)?;

                match (reference, expression) {
                    (
                        Reference::Id(id),
                        Expression::Function {
                            name: Option::None,
                            arguments,
                            statements,
                        },
                    ) => Ok(Expression::Assign {
                        assign_to: Reference::Id(id),
                        expression: Box::new(Expression::Function {
                            name: Some(id),
                            arguments,
                            statements,
                        }),
                    }),
                    (
                        Reference::Accessor {
                            expression,
                            accessor,
                            null_safe,
                        },
                        Expression::Function {
                            name: Option::None,
                            arguments,
                            statements,
                        },
                    ) => Ok(Expression::Assign {
                        assign_to: Reference::Accessor {
                            expression,
                            accessor,
                            null_safe,
                        },
                        expression: Box::new(Expression::Function {
                            name: Some(accessor),
                            arguments,
                            statements,
                        }),
                    }),
                    (reference, expression) => Ok(Expression::Assign {
                        assign_to: reference,
                        expression: Box::new(expression),
                    }),
                }
            }
            Some((Token::AddAssign, ..)) => {
                input.next();
                as_op_assign(input, reference, context, BinaryOperator::Add)
            }
            Some((Token::SubAssign, ..)) => {
                input.next();
                as_op_assign(input, reference, context, BinaryOperator::Sub)
            }
            Some((Token::MulAssign, ..)) => {
                input.next();
                as_op_assign(input, reference, context, BinaryOperator::Mul)
            }
            Some((Token::DivAssign, ..)) => {
                input.next();
                as_op_assign(input, reference, context, BinaryOperator::Div)
            }
            Some((Token::ModAssign, ..)) => {
                input.next();
                as_op_assign(input, reference, context, BinaryOperator::Mod)
            }
            Some((Token::ExpAssign, ..)) => {
                input.next();
                as_op_assign(input, reference, context, BinaryOperator::Exponential)
            }
            _ => Ok(Expression::Reference(reference)),
        },
        other => Ok(other),
    }
}

// fn parse_function_expression<'a>(
//     input: &mut LexerImpl<'a>,
//     context: ParseContext,
// ) -> Result<'a, Expression<'a>> {
//     if input.consume_if(Token::Function) {
//         let arguments = parse_args_list(input)?;
//         let statements = BlockStatement::parse(input, ParseContext { top_level: false })?;
//
//         Ok(Expression::Function {
//             name: None,
//             arguments,
//             statements,
//         })
//     } else {
//         parse_assignment(input, context)
//     }
// }

pub(crate) fn parse_expression<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, Expression<'a>> {
    parse_assignment(input, context)
    // parse_function_expression(input, context)
}

impl<'a> Parse<'a> for IfStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.expect(Token::If)?;

        let condition = parse_group(input, context)?;
        let true_block = Statement::parse(input, context)?;

        let false_block = if input.consume_if(Token::Else) {
            Some(Box::new(Statement::parse(input, context)?))
        } else {
            None
        };

        Ok(IfStatement {
            condition,
            true_block: Box::new(true_block),
            false_block,
        })
    }
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

impl<'a> Parse<'a> for VarStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, VarStatement<'a>> {
        input.expect(Token::Var)?;

        let mut declarations = Vec::new();
        loop {
            let identifier = input.expect_id()?;

            let expression = if input.consume_if(Token::Assign) {
                Some(parse_expression(input, context)?)
            } else {
                None
            };

            declarations.push(VarDeclaration {
                identifier,
                expression,
            });

            if !input.consume_if(Token::Comma) {
                break;
            }
        }

        Ok(VarStatement { declarations })
    }
}

impl<'a> Parse<'a> for ConstStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.expect(Token::Const)?;

        let mut declarations = Vec::new();
        loop {
            let identifier = input.expect_id()?;

            let expression = if input.consume_if(Token::Assign) {
                Some(parse_expression(input, context)?)
            } else {
                None
            };

            declarations.push(VarDeclaration {
                identifier,
                expression,
            });

            if !input.consume_if(Token::Comma) {
                break;
            }
        }

        Ok(ConstStatement { declarations })
    }
}

fn parse_args_list<'a>(input: &mut LexerImpl<'a>) -> Result<'a, Vec<&'a str>> {
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

fn parse_group<'a>(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Expression<'a>> {
    input.expect(Token::OpenParen)?;

    let expr = parse_expression(input, context)?;

    input.expect(Token::CloseParen)?;

    Ok(expr)
}

impl<'a> Parse<'a> for BlockStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.next();

        let mut statements = Vec::new();
        while !input.lookahead_is(Token::CloseBrace) {
            statements.push(DeclStatement::parse(input, context)?);

            if input.lookahead_is(Token::Semicolon) {
                input.next();
            }
        }

        input.expect(Token::CloseBrace)?;

        Ok(BlockStatement { statements })
    }
}

impl<'a> Parse<'a> for TryStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.expect(Token::Try)?;

        let try_block = BlockStatement::parse(input, context)?;

        let (catch_binding, catch_block) = if input.consume_if(Token::Catch) {
            let binding = if input.consume_if(Token::OpenParen) {
                let id = input.expect_id()?;
                input.expect(Token::CloseParen)?;
                Some(id)
            } else {
                None
            };

            let block = BlockStatement::parse(input, context)?;

            (binding, Some(block))
        } else {
            (None, None)
        };

        let finally_block = if input.consume_if(Token::Finally) {
            Some(BlockStatement::parse(input, context)?)
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

impl<'a> Parse<'a> for ClassStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        input.expect(Token::Class)?;

        let name = input.expect_id()?;

        let extends = if input.consume_if(Token::Extends) {
            Some(Expression::parse(input, context)?)
        } else {
            None
        };

        input.expect(Token::OpenBrace)?;

        let mut members = Vec::new();
        let mut private_members = Vec::new();
        while !input.consume_if(Token::CloseBrace) {
            // let is_static = input.consume_if(Token::Static);
            //
            // let is_getter = if matches!(input.lookahead(), Some((Token::Id("get"), ..))) {
            //     input.next();
            //     true
            // } else {
            //     false
            // };
            //
            // let is_setter = if matches!(input.lookahead(), Some((Token::Id("set"), ..))) {
            //     input.next();
            //     true
            // } else {
            //     false
            // };

            match input.lookahead() {
                Some((Token::Id(identifier), ..)) => {
                    input.next();

                    let arguments = parse_args_list(input)?;
                    let statements = BlockStatement::parse(input, context)?;

                    members.push(ClassMember::Function(FunctionStatement {
                        identifier,
                        arguments,
                        statements,
                    }))
                }
                Some((Token::PrivateId(identifier), ..)) => {
                    input.next();

                    private_members.push(ClassMember::PrivateField(Field { identifier }))
                }
                Some((Token::Static, ..)) => {
                    input.next();

                    let identifier = input.expect_id()?;
                    let arguments = parse_args_list(input)?;
                    let statements = BlockStatement::parse(input, context)?;

                    members.push(ClassMember::StaticFunction(FunctionStatement {
                        identifier,
                        arguments,
                        statements,
                    }))
                }
                Some(token) => return input.expected(&[Token::Id("constructor")], token),
                _ => panic!(":("),
            }

            input.consume_if(Token::Semicolon);
        }

        Ok(ClassStatement {
            name,
            extends,
            members,
        })
    }
}

impl<'a> Parse<'a> for ReturnStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, ReturnStatement<'a>> {
        input.expect(Token::Return)?;

        if input.lookahead_is(Token::Semicolon) {
            Ok(ReturnStatement { expression: None })
        } else {
            let expression = parse_expression(input, context)?;

            Ok(ReturnStatement {
                expression: Some(expression),
            })
        }
    }
}

impl<'a> Parse<'a> for FunctionStatement<'a> {
    fn parse(
        input: &mut LexerImpl<'a>,
        context: ParseContext,
    ) -> Result<'a, FunctionStatement<'a>> {
        input.expect(Token::Function)?;
        let identifier = input.expect_id()?;
        let arguments = parse_args_list(input)?;
        let statements = BlockStatement::parse(input, ParseContext { top_level: false })?;

        Ok(FunctionStatement {
            identifier,
            arguments,
            statements,
        })
    }
}

impl<'a> Parse<'a> for ThrowStatement<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, ThrowStatement<'a>> {
        input.expect(Token::Throw)?;
        let expression = parse_expression(input, context)?;

        Ok(ThrowStatement { expression })
    }
}

pub(crate) fn parse<'a>(
    input: &mut LexerImpl<'a>,
    context: ParseContext,
) -> Result<'a, ParsedModule<'a>> {
    let mut statements = Vec::new();
    while input.lookahead().is_some() {
        statements.push(DeclStatement::parse(input, context)?);

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
    use crate::parser::hand_parser::{BlockStatement, FunctionStatement};
    use logos::Logos;

    #[test]
    fn test_parse_function() {
        assert_eq!(
            FunctionStatement::parse(
                &mut WhitespaceTrackingLexer {
                    lexer: Token::lexer("function hello(world, boom) { }").spanned(),
                    peeked: None,
                    previous_was_newline: false
                },
                ParseContext { top_level: true }
            ),
            Ok(FunctionStatement {
                identifier: "hello",
                arguments: vec!["world", "boom"],
                statements: BlockStatement { statements: vec![] }
            })
        );
    }

    // #[test]
    // fn test_parse_block() {
    //     assert_eq!(
    //         BlockStatement::parse(&mut Token::lexer("{ console?.log('hi'); }").spanned().peekable()),
    //         Ok(vec![Statement::Expression(Expression::Call {
    //             expression: Box::new(Expression::Reference(Reference::Accessor {
    //                 expression: Box::new(Expression::Reference(Reference::Id("console"))),
    //                 accessor: "log",
    //                 null_safe: true
    //             })),
    //             parameters: vec![Expression::String("'hi'")]
    //         })])
    //     );
    // }

    #[test]
    fn test_operators() {
        assert_eq!(
            parse_expression(
                &mut WhitespaceTrackingLexer {
                    lexer: Token::lexer("1.0 + Test").spanned(),
                    peeked: None,
                    previous_was_newline: false
                },
                ParseContext { top_level: true }
            ),
            Ok(Expression::BinaryExpression {
                left: Box::new(Expression::Float(1.0)),
                right: Box::new(Expression::Reference(Reference::Id("Test"))),
                operator: BinaryOperator::Add
            })
        );
    }

    // #[test]
    // fn test_if_else() {
    //     assert_eq!(
    //         IfStatement::parse(
    //             &mut Token::lexer("if (true) { return 1; } else { return; }")
    //                 .spanned()
    //                 .peekable()
    //         ),
    //         Ok(IfStatement {
    //             condition: Expression::Boolean(true),
    //             true_block: vec![Statement::Return(ReturnStatement {
    //                 expression: Some(Expression::Float(1.0))
    //             })],
    //             false_block: Some(vec![Statement::Return(ReturnStatement {
    //                 expression: Some(Expression::Float(2.0))
    //             })])
    //         })
    //     );
    // }
}
