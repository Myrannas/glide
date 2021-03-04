use crate::value::{Reference, RuntimeValue};
use colored::Colorize;
use std::cmp::Ordering;
use std::fmt::{Formatter, Result, Write};

pub struct Renderer<'a, 'b> {
    max_depth: usize,
    current_depth: usize,
    pub(crate) representation: Representation,
    pub(crate) formatter: &'b mut Formatter<'a>,
}

impl<'a, 'b> Renderer<'a, 'b> {
    pub fn render(&mut self, object: &dyn DebugRepresentation) -> Result {
        match self.current_depth.cmp(&self.max_depth) {
            Ordering::Equal | Ordering::Greater => {
                object.render(&mut Renderer::compact(self.formatter))
            }
            Ordering::Less => object.render(&mut Renderer {
                max_depth: self.max_depth,
                current_depth: self.current_depth + 1,
                formatter: self.formatter,
                representation: self.representation,
            }),
        }
    }

    pub(crate) fn compact(formatter: &'b mut Formatter<'a>) -> Self {
        Renderer {
            max_depth: 0,
            current_depth: 1,
            formatter,
            representation: Representation::Compact,
        }
    }

    pub fn full(formatter: &'b mut Formatter<'a>) -> Self {
        Renderer {
            max_depth: 3,
            current_depth: 0,
            formatter,
            representation: Representation::Full,
        }
    }

    pub(crate) fn debug(formatter: &'b mut Formatter<'a>, depth: usize) -> Self {
        Renderer {
            max_depth: depth,
            current_depth: 0,
            formatter,
            representation: Representation::Debug,
        }
    }

    #[inline]
    pub(crate) fn start_internal(&mut self, internal_type: &str) -> Result {
        self.formatter.write_fmt(format_args!(
            "{}{}{}",
            "[[".blue(),
            internal_type.blue(),
            "| ".blue()
        ))
    }

    #[inline]
    pub(crate) fn end_internal(&mut self) -> Result {
        self.formatter.write_fmt(format_args!("{}", "]]".blue()))
    }

    #[inline]
    pub(crate) fn internal_key(&mut self, key: &str) -> Result {
        self.formatter.write_fmt(format_args!("{}: ", key.blue()))
    }

    #[inline]
    pub(crate) fn new_line(&mut self) -> Result {
        self.formatter.write_char('\n')
    }

    #[inline]
    pub(crate) fn internal_index(&mut self, index: usize) -> Result {
        self.formatter
            .write_fmt(format_args!("{}: ", index.to_string().blue()))
    }

    #[inline]
    pub(crate) fn literal(&mut self, value: &str) -> Result {
        self.formatter
            .write_fmt(format_args!("{}", value.bright_yellow()))
    }

    #[inline]
    pub(crate) fn string_literal(&mut self, value: &str) -> Result {
        self.formatter
            .write_fmt(format_args!("\"{}\"", value.bright_yellow()))
    }

    #[inline]
    pub(crate) fn instruction(&mut self, value: &str) -> Result {
        self.formatter
            .write_fmt(format_args!("{:10.10}", value.magenta()))
    }

    #[inline]
    pub(crate) fn function(&mut self, name: &str) -> Result {
        self.formatter.write_fmt(format_args!(
            "{}{}{}",
            "[Function: ".green(),
            name.green(),
            "]".green()
        ))
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Representation {
    Compact,
    Full,
    Debug,
}

pub trait DebugRepresentation {
    fn render(&self, renderer: &mut Renderer) -> Result;
}

impl<'a> DebugRepresentation for RuntimeValue<'a> {
    fn render(&self, render: &mut Renderer) -> Result {
        match (&render.representation, self) {
            (.., RuntimeValue::Boolean(true)) => render.literal("true"),
            (.., RuntimeValue::Boolean(false)) => render.literal("false"),
            (.., RuntimeValue::Undefined) => render.literal("undefined"),
            (.., RuntimeValue::Null) => render.literal("null"),
            (.., RuntimeValue::Object(obj)) => render.render(obj),
            (.., RuntimeValue::Function(_, obj)) => render.render(obj),
            (.., RuntimeValue::String(str, ..)) => render.string_literal(str),
            (Representation::Debug, RuntimeValue::Reference(reference)) => {
                render.start_internal("REF")?;
                RuntimeValue::render(&reference.base, render)?;

                render.formatter.write_fmt(format_args!(
                    "{}{}",
                    if reference.strict { "." } else { "?." },
                    &reference.name,
                ));

                render.end_internal()?;
                Ok(())
            }
            (Representation::Compact, RuntimeValue::Reference(reference)) => match &*reference.base
            {
                RuntimeValue::Object(obj) => {
                    render
                        .formatter
                        .write_fmt(format_args!(".{}", reference.name))?;

                    Ok(())
                }
                RuntimeValue::Function(_, obj) => {
                    render
                        .formatter
                        .write_fmt(format_args!(".{}", reference.name))?;

                    Ok(())
                }

                value => panic!("Unsupported object type {:?}", value),
            },
            (.., RuntimeValue::Float(value)) => render.literal(&format!("{}", value)),
            other => panic!("Unsupported debug view"),
        }
    }
}
