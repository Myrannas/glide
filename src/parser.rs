use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, char, multispace0, multispace1};
use nom::combinator::{cut, map, value};
use nom::error::{context, convert_error, ContextError, ParseError, VerboseError};
use nom::multi::{fold_many0, many0, separated_list0};
use nom::number::complete::double;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{Err, IResult};

fn id<'a, E: ParseError<&'a str> + ContextError<&'a str>>(i: &'a str) -> IResult<&'a str, Ast, E> {
    map(
        context("identifier", preceded(multispace0, alpha1)),
        Ast::Id,
    )(i)
}

fn number<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Ast, E> {
    map(context("float", preceded(multispace0, double)), Ast::Float)(i)
}

fn boolean<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Ast, E> {
    let parse_true = value(true, tag("true"));
    let parse_false = value(false, tag("false"));

    map(
        context(
            "float",
            preceded(multispace0, alt((parse_true, parse_false))),
        ),
        Ast::Boolean,
    )(i)
}

fn parse_value<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Ast, E> {
    alt((parse_call, boolean, id, number))(i)
}

fn parse_call<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Ast, E> {
    map(
        tuple((
            alpha1,
            delimited(
                char('('),
                separated_list0(char(','), parse_expression),
                char(')'),
            ),
        )),
        |(id, expressions)| Ast::Call(id, expressions),
    )(i)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast<'a> {
    Add(Box<Ast<'a>>, Box<Ast<'a>>),
    Sub(Box<Ast<'a>>, Box<Ast<'a>>),
    Mul(Box<Ast<'a>>, Box<Ast<'a>>),
    Div(Box<Ast<'a>>, Box<Ast<'a>>),
    Mod(Box<Ast<'a>>, Box<Ast<'a>>),
    Return(Box<Ast<'a>>),
    Var(&'a str, Box<Ast<'a>>),
    Id(&'a str),
    Float(f64),
    Function(&'a str, Vec<&'a str>, Vec<Ast<'a>>),
    Boolean(bool),
    Call(&'a str, Vec<Ast<'a>>),
    IfStatement {
        condition: Box<Ast<'a>>,
        block: Vec<Ast<'a>>,
    },
    Statement(Box<Ast<'a>>),
}

fn multiplicative<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&str, Ast, E> {
    let operator = alt((char('*'), char('/'), char('%')));
    let repeated_element = pair(
        preceded(multispace0, operator),
        preceded(multispace0, parse_value),
    );

    let (input, first) = parse_value(input)?;

    fold_many0(repeated_element, first, |acc, (op, id)| match op {
        '*' => Ast::Mul(Box::new(acc), Box::new(id)),
        '/' => Ast::Div(Box::new(acc), Box::new(id)),
        '%' => Ast::Mod(Box::new(acc), Box::new(id)),
        _ => panic!("Unsupported operation"),
    })(input)
}

fn additive<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Ast, E> {
    let operator = alt((char('+'), char('-')));
    let repeated_element = pair(preceded(multispace0, operator), multiplicative);

    let (input, first) = multiplicative(input)?;

    fold_many0(repeated_element, first, |acc, (op, id)| match op {
        '+' => Ast::Add(Box::new(acc), Box::new(id)),
        '-' => Ast::Sub(Box::new(acc), Box::new(id)),
        _ => panic!("Unsupported operation"),
    })(input)
}

fn parse_expression<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Ast, E> {
    additive(input)
}

fn parse_if_statement<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Ast, E> {
    map(
        preceded(
            tag("if"),
            tuple((delimited(tag("("), parse_expression, tag(")")), parse_block)),
        ),
        |(condition, block)| Ast::IfStatement {
            block,
            condition: Box::new(condition),
        },
    )(input)
}

fn parse_statement<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Ast, E> {
    map(
        preceded(
            multispace0,
            terminated(
                alt((
                    parse_assignment,
                    parse_if_statement,
                    parse_function,
                    parse_return,
                    parse_expression,
                )),
                char(';'),
            ),
        ),
        |v| Ast::Statement(Box::new(v)),
    )(input)
}

fn parse_statements<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&str, Vec<Ast>, E> {
    delimited(multispace0, many0(parse_statement), multispace0)(input)
}

fn parse_block<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<Ast>, E> {
    delimited(char('{'), parse_statements, char('}'))(input)
}

fn parse_function<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Ast, E> {
    let function_start = tuple((tag("function"), multispace1));
    map(
        preceded(
            function_start,
            cut(tuple((
                alpha1,
                delimited(char('('), separated_list0(char(','), alpha1), char(')')),
                preceded(multispace0, parse_block),
            ))),
        ),
        |(name, arguments, statements)| Ast::Function(name, arguments, statements),
    )(input)
}

fn parse_return<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Ast, E> {
    map(
        context(
            "return",
            preceded(tuple((tag("return"), multispace1)), cut(parse_expression)),
        ),
        |expression| Ast::Return(Box::new(expression)),
    )(input)
}

fn parse_assignment<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&str, Ast, E> {
    map(
        tuple((
            preceded(tuple((tag("var"), multispace1)), alpha1),
            preceded(tuple((multispace0, char('='))), parse_expression),
        )),
        |(variable, value)| Ast::Var(variable, Box::new(value)),
    )(input)
}

#[derive(Debug)]
pub(crate) struct ParsedModule<'a> {
    pub(crate) statements: Vec<Ast<'a>>,
}

pub(crate) fn parse_module(input: &str) -> ParsedModule {
    match parse_statements::<VerboseError<&str>>(input) {
        Ok((result, ast)) => {
            assert_eq!(result, "");
            ParsedModule { statements: ast }
        }
        Err(Err::Error(e)) | Err(Err::Failure(e)) => panic!("{}", convert_error(input, e)),
        _ => panic!(),
    }
}

#[cfg(test)]
mod tests {
    use super::Ast::*;
    use super::*;

    #[test]
    fn test_id_parser() {
        assert_eq!(id::<VerboseError<&str>>("test"), Ok(("", Ast::Id("test"))));
        assert_eq!(id::<VerboseError<&str>>(" test"), Ok(("", Ast::Id("test"))));
    }

    #[test]
    fn test_add_parser() {
        assert_eq!(
            additive::<VerboseError<&str>>("a + b"),
            Ok(("", Add(Box::new(Id("a")), Box::new(Id("b")))))
        );
        assert_eq!(
            additive::<VerboseError<&str>>("a - b + c"),
            Ok((
                "",
                Add(
                    Box::new(Ast::Sub(Box::new(Id("a")), Box::new(Id("b")))),
                    Box::new(Id("c"))
                )
            ))
        );
        assert_eq!(
            additive::<VerboseError<&str>>("a - b * c"),
            Ok((
                "",
                Ast::Sub(
                    Box::new(Id("a")),
                    Box::new(Ast::Mul(Box::new(Id("b")), Box::new(Id("c")))),
                )
            ))
        );
    }

    #[test]
    fn test_parse_function() {
        assert_eq!(
            parse_function::<VerboseError<&str>>("function test() { return a; }"),
            Ok((
                "",
                Function(
                    "test",
                    Vec::new(),
                    vec![Ast::Statement(Box::new(Ast::Return(Box::new(Id("a")))))]
                )
            ))
        );
    }
}
