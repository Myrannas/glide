use crate::JsThread;
use colored::Colorize;
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter, Result, Write};

pub struct Renderer<'a, 'b, 'c, 'd> {
    max_depth: usize,
    current_depth: usize,
    pub(crate) representation: Representation,
    pub(crate) formatter: &'b mut Formatter<'c>,
    pub(crate) thread: &'d JsThread<'a>,
}

impl<'a, 'b, 'c, 'd> Renderer<'a, 'b, 'c, 'd> {
    pub fn render(&mut self, object: &dyn DebugRepresentation<'a>) -> Result {
        match self.current_depth.cmp(&self.max_depth) {
            Ordering::Equal | Ordering::Greater => {
                let representation = self.representation;
                self.representation = Representation::Compact;
                let result = object.render(self);
                self.representation = representation;

                result
            }
            Ordering::Less => {
                self.current_depth += 1;

                let result = object.render(self);

                self.current_depth -= 1;

                result
            }
        }
    }

    pub(crate) fn compact(formatter: &'b mut Formatter<'c>, thread: &'d JsThread<'a>) -> Self {
        Renderer {
            max_depth: 0,
            current_depth: 1,
            formatter,
            representation: Representation::Compact,
            thread,
        }
    }

    pub(crate) fn debug(
        formatter: &'b mut Formatter<'c>,
        thread: &'d JsThread<'a>,
        depth: usize,
    ) -> Self {
        Renderer {
            max_depth: depth,
            current_depth: 0,
            formatter,
            representation: Representation::Debug,
            thread,
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

pub trait DebugRepresentation<'a> {
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> Result;

    // fn debuggable<'a, 'b>(&'a self, thread: &'a JsThread<'b>) -> Debuggable<'a, 'b, &'a Self> {
    //     Debuggable {
    //         debuggable: self,
    //         thread,
    //     }
    // }
}

pub(crate) struct DebuggableWithThread<'a, 'b, 'c> {
    value: &'c dyn DebugRepresentation<'a>,
    thread: &'b JsThread<'a>,
}

impl<'a, 'b, 'c> DebuggableWithThread<'a, 'b, 'c> {
    pub(crate) fn from(value: &'c dyn DebugRepresentation<'a>, thread: &'b JsThread<'a>) -> Self {
        DebuggableWithThread { value, thread }
    }
}

impl<'a, 'b, 'c> Debug for DebuggableWithThread<'a, 'b, 'c> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut renderer = Renderer::debug(f, self.thread, 3);

        renderer.render(self.value)
    }
}
