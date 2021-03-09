use crate::{JsPrimitiveString, RuntimeValue};

#[derive(Clone, Debug)]
pub enum JsPrimitive {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(JsPrimitiveString),
}

impl<'a> From<JsPrimitive> for RuntimeValue<'a> {
    fn from(value: JsPrimitive) -> Self {
        match value {
            JsPrimitive::Undefined => RuntimeValue::Undefined,
            JsPrimitive::Null => RuntimeValue::Null,
            JsPrimitive::Boolean(value) => RuntimeValue::Boolean(value),
            JsPrimitive::Float(value) => RuntimeValue::Float(value),
            JsPrimitive::String(str) => RuntimeValue::String(str),
        }
    }
}

impl<'a> From<RuntimeValue<'a>> for JsPrimitive {
    fn from(value: RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Undefined => JsPrimitive::Undefined,
            RuntimeValue::Null => JsPrimitive::Null,
            RuntimeValue::Boolean(b) => JsPrimitive::Boolean(b),
            RuntimeValue::Float(f) => JsPrimitive::Float(f),
            RuntimeValue::String(str) => JsPrimitive::String(str),
            RuntimeValue::Object(_) => panic!("Cannot be wrapped"),
            RuntimeValue::Reference(_) => panic!("Cannot be wrapped"),
            RuntimeValue::Internal(_) => panic!("Cannot be wrapped"),
        }
    }
}
