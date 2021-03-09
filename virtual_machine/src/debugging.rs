use crate::value::{InternalValue, RuntimeValue};
use crate::{JsObject, JsThread};
use colored::Colorize;
use std::cmp::Ordering;
use std::fmt::{Formatter, Result, Write};

pub struct Renderer<'a, 'b, 'c, 'd> {
    max_depth: usize,
    current_depth: usize,
    pub(crate) representation: Representation,
    pub(crate) formatter: &'b mut Formatter<'a>,
    thread: Option<&'d JsThread<'c>>,
}

impl<'a, 'b, 'c, 'd> Renderer<'a, 'b, 'c, 'd> {
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
                thread: self.thread,
            }),
        }
    }

    pub(crate) fn compact(formatter: &'b mut Formatter<'a>) -> Self {
        Renderer {
            max_depth: 0,
            current_depth: 1,
            formatter,
            representation: Representation::Compact,
            thread: None,
        }
    }

    pub(crate) fn debug(formatter: &'b mut Formatter<'a>, depth: usize) -> Self {
        Renderer {
            max_depth: depth,
            current_depth: 0,
            formatter,
            representation: Representation::Debug,
            thread: None,
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

    pub(crate) fn with_thread(mut self, thread: &'d JsThread<'c>) -> Self {
        self.thread = Some(thread);
        self
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
    Debug,
}

// struct Debuggable<'a, 'b, A> {
//     debuggable: &'a A,
//     thread: &'a JsThread<'b>,
// }
//
// impl DebugRepresentation for Debuggable<'a, 'b, A> {
//     fn render(&self, renderer: &mut Renderer<'a, 'b, 'c, 'd>) -> Result {
//         unimplemented!()
//     }
// }

pub trait DebugRepresentation {
    fn render(&self, renderer: &mut Renderer) -> Result;

    // fn debuggable<'a, 'b>(&'a self, thread: &'a JsThread<'b>) -> Debuggable<'a, 'b, &'a Self> {
    //     Debuggable {
    //         debuggable: self,
    //         thread,
    //     }
    // }
}

impl<'a> DebugRepresentation for RuntimeValue<'a> {
    fn render(&self, render: &mut Renderer) -> Result {
        match (&render.representation, self) {
            (.., RuntimeValue::Boolean(true)) => render.literal("true"),
            (.., RuntimeValue::Boolean(false)) => render.literal("false"),
            (.., RuntimeValue::Undefined) => render.literal("undefined"),
            (.., RuntimeValue::Null) => render.literal("null"),
            (.., RuntimeValue::Object(obj)) => render.render(obj),
            (.., RuntimeValue::String(str)) => render.string_literal(str.as_ref()),
            (Representation::Debug, RuntimeValue::Reference(reference)) => {
                render.start_internal("REF")?;
                JsObject::render(&reference.base, render)?;

                render.formatter.write_fmt(format_args!(
                    "{}{}",
                    if reference.strict { "." } else { "?." },
                    &reference.name,
                ))?;

                render.end_internal()?;
                Ok(())
            }
            (Representation::Debug, RuntimeValue::Internal(InternalValue::Local(local))) => {
                render.start_internal("INTERNAL")?;
                render.internal_key("index")?;
                render.literal(&local.to_string())?;
                render.end_internal()?;
                Ok(())
            }
            (Representation::Compact, RuntimeValue::Reference(reference)) => {
                render
                    .formatter
                    .write_fmt(format_args!(".{}", reference.name))?;

                Ok(())
            }
            (.., RuntimeValue::Float(value)) => render.literal(&format!("{}", value)),
            other => panic!("Unsupported debug view"),
        }
    }
}
