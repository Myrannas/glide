use crate::parser::ast::{BlockStatement, Expression, Field, FunctionStatement};
use crate::parser::hand_parser::{parse_args_list, Result};
use crate::parser::hand_parser::{LexerImpl, Parse};
use crate::parser::lexer::Token;
use crate::parser::ParseContext;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ClassStatement<'a> {
    pub(crate) name: &'a str,
    pub(crate) extends: Option<Expression<'a>>,
    pub(crate) members: Vec<ClassMember<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ClassMember<'a> {
    Constructor(FunctionStatement<'a>),
    Function {
        function: FunctionStatement<'a>,
        is_static: bool,
    },
    PrivateField {
        field: Field<'a>,
        is_static: bool,
    },
    Getter {
        function: FunctionStatement<'a>,
        is_static: bool,
    },
    Setter {
        function: FunctionStatement<'a>,
        is_static: bool,
    },
}

impl<'a> Parse<'a> for ClassMember<'a> {
    fn parse(input: &mut LexerImpl<'a>, context: ParseContext) -> Result<'a, Self> {
        let is_static = input.consume_if(Token::Static);

        match input.lookahead() {
            Some((Token::Id(identifier), ..)) => {
                input.next();

                let arguments = parse_args_list(input)?;
                let statements = BlockStatement::parse(input, context)?;

                Ok(ClassMember::Function {
                    function: FunctionStatement {
                        identifier,
                        arguments,
                        statements,
                    },
                    is_static,
                })
            }
            Some((Token::PrivateId(identifier), ..)) => {
                input.next();

                Ok(ClassMember::PrivateField {
                    field: Field { identifier },
                    is_static,
                })
            }
            Some((Token::Get, ..)) => {
                input.next();

                let identifier = input.expect_id()?;
                input.expect(Token::OpenParen)?;
                input.expect(Token::CloseParen)?;
                let statements = BlockStatement::parse(input, ParseContext { top_level: false })?;

                Ok(ClassMember::Getter {
                    is_static,
                    function: FunctionStatement {
                        identifier,
                        statements,
                        arguments: vec![],
                    },
                })
            }
            Some((Token::Set, ..)) => {
                input.next();

                let identifier = input.expect_id()?;
                let arguments = parse_args_list(input)?;
                let statements = BlockStatement::parse(input, ParseContext { top_level: false })?;

                Ok(ClassMember::Setter {
                    is_static,
                    function: FunctionStatement {
                        identifier,
                        statements,
                        arguments,
                    },
                })
            }
            Some(token) => return input.expected(&[Token::Id("constructor")], token),
            _ => panic!(":("),
        }
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
        while !input.consume_if(Token::CloseBrace) {
            members.push(ClassMember::parse(input, context)?);

            input.consume_if(Token::Semicolon);
        }

        Ok(ClassStatement {
            name,
            extends,
            members,
        })
    }
}
