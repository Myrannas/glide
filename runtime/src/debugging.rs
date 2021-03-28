use crate::result::JsResult;
use crate::{ExecutionError, JsThread, Realm};
use colored::Colorize;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter, Result, Write};

pub struct Renderer<'a, 'b, 'c, 'd> {
    max_depth: usize,
    current_depth: usize,
    pub(crate) representation: Representation,
    pub(crate) formatter: &'b mut Formatter<'c>,
    pub(crate) realm: &'d Realm<'a>,
}

#[allow(dead_code)]
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

    pub(crate) fn compact(formatter: &'b mut Formatter<'c>, realm: &'d Realm<'a>) -> Self {
        Renderer {
            max_depth: 0,
            current_depth: 1,
            formatter,
            representation: Representation::Compact,
            realm,
        }
    }

    pub(crate) fn debug(
        formatter: &'b mut Formatter<'c>,
        realm: &'d Realm<'a>,
        depth: usize,
    ) -> Self {
        Renderer {
            max_depth: depth,
            current_depth: 0,
            formatter,
            representation: Representation::Debug,
            realm,
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

pub(crate) struct X<'a, 'b, 'c> {
    value: &'c dyn DebugRepresentation<'a>,
    realm: &'b Realm<'a>,
}

impl<'a, 'b, 'c> X<'a, 'b, 'c> {
    pub(crate) fn from(value: &'c dyn DebugRepresentation<'a>, realm: &'b Realm<'a>) -> Self {
        X { value, realm }
    }
}

pub(crate) trait DebugWithRealm<'a, 'b, 'c> {
    fn debug_value(&'b self, value: &'c dyn DebugRepresentation<'a>) -> X<'a, 'b, 'c>;
}

impl<'a, 'b, 'c> DebugWithRealm<'a, 'b, 'c> for JsThread<'a> {
    fn debug_value(&'b self, value: &'c dyn DebugRepresentation<'a>) -> X<'a, 'b, 'c> {
        X::from(value, self.get_realm())
    }
}

impl<'a, 'b, 'c> DebugWithRealm<'a, 'b, 'c> for Realm<'a> {
    fn debug_value(&'b self, value: &'c dyn DebugRepresentation<'a>) -> X<'a, 'b, 'c> {
        X::from(value, self)
    }
}

impl<'a, 'b, 'c> Debug for X<'a, 'b, 'c> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut renderer = Renderer::debug(f, &self.realm, 5);

        renderer.render(self.value)
    }
}

impl<'a, 'b, 'c> Display for X<'a, 'b, 'c> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let mut renderer = Renderer::compact(f, &self.realm);

        renderer.render(self.value)
    }
}

impl<'a, T> DebugRepresentation<'a> for Vec<T>
where
    T: DebugRepresentation<'a>,
{
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> Result {
        renderer.formatter.write_char('[')?;

        let mut after_first = false;
        for item in self {
            if after_first {
                renderer.formatter.write_str(", ")?;
            } else {
                after_first = true
            }

            item.render(renderer)?;
        }

        renderer.formatter.write_char(']')?;
        Ok(())
    }
}

impl<'a, T> DebugRepresentation<'a> for Option<T>
where
    T: DebugRepresentation<'a>,
{
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> Result {
        match self {
            Some(value) => renderer.render(value)?,
            None => {}
        }

        Ok(())
    }
}

pub trait Unwrap<'a, T> {
    fn expect_value(self, realm: &Realm<'a>, message: &str) -> T;
    fn unwrap_value(self, realm: &Realm<'a>) -> T;
}

impl<'a, T> Unwrap<'a, T> for JsResult<'a, T> {
    fn expect_value(self, realm: &Realm<'a>, message: &str) -> T {
        match self {
            Ok(result) => result,
            Err(ExecutionError::Thrown(value, _)) => {
                panic!("{:?}", realm.debug_value(&value))
            }
            Err(ExecutionError::SyntaxError(syntax_error)) => {
                panic!("{:?}", syntax_error)
            }
            Err(ExecutionError::InternalError(internal_error)) => {
                panic!("{:?}", internal_error)
            }
        }
    }

    fn unwrap_value(self, realm: &Realm<'a>) -> T {
        self.expect_value(realm, "Expected a value")
    }
}
