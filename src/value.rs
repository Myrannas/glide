use crate::ops::{Context, ContextAccess, RuntimeFrame};
use crate::vm::Function;
use std::cell::RefCell;
use std::f64::NAN;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Reference<'a> {
    base: RuntimeValue<'a>,
    name: String,
    strict: bool,
}

#[derive(Clone, Debug)]
pub enum StaticValue {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(String),
    Local(usize),
    Capture(usize, usize),
    Function(usize),
}

impl<'a> StaticValue {
    pub(crate) fn to_runtime<'b>(&self, frame: &'b RuntimeFrame<'a>) -> RuntimeValue<'a> {
        match self {
            StaticValue::Undefined => RuntimeValue::Undefined,
            StaticValue::Null => RuntimeValue::Null,
            StaticValue::Boolean(v) => RuntimeValue::Boolean(*v),
            StaticValue::Float(f) => RuntimeValue::Float(*f),
            StaticValue::String(s) => RuntimeValue::String(s.clone()),
            StaticValue::Local(l) => frame.context.read(*l),
            StaticValue::Function(f) => {
                RuntimeValue::Function(&frame.function.functions[*f], frame.context.clone())
            }
            StaticValue::Capture(offset, index) => frame
                .context
                .capture(*offset, *index)
                .expect("Expected capture to work"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum RuntimeValue<'a> {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(String),
    Object,
    Reference(Box<Reference<'a>>),
    Local(usize),
    Function(&'a Function, Rc<RefCell<Context<'a>>>),
}

impl<'a> Into<f64> for RuntimeValue<'a> {
    fn into(self) -> f64 {
        match self {
            RuntimeValue::Undefined => NAN,
            RuntimeValue::Null => 0.0,
            RuntimeValue::Boolean(true) => 1.0,
            RuntimeValue::Boolean(false) => 0.0,
            RuntimeValue::Float(v) => v,
            value => todo!("Unsupported types {:?}", value),
        }
    }
}

enum Error {
    ReferenceError,
}

impl<'a> Reference<'a> {
    fn resolve(self) -> Result<RuntimeValue<'a>, Error> {
        match self.base {
            RuntimeValue::Undefined => Err(Error::ReferenceError),
            _ => Err(Error::ReferenceError),
        }
    }
}
