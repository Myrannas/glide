use crate::values::nan::{Value, ValueType};
use crate::JsPrimitiveString;

#[derive(Clone, Debug)]
pub enum JsPrimitive {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(JsPrimitiveString),
}

impl<'a> From<JsPrimitive> for Value<'a> {
    fn from(value: JsPrimitive) -> Self {
        match value {
            JsPrimitive::Undefined => Value::UNDEFINED,
            JsPrimitive::Null => Value::NULL,
            JsPrimitive::Boolean(value) => Value::from(value),
            JsPrimitive::Float(value) => Value::from(value),
            JsPrimitive::String(str) => Value::from(str),
        }
    }
}

impl<'a> From<Value<'a>> for JsPrimitive {
    fn from(value: Value<'a>) -> Self {
        match value.get_type() {
            ValueType::Undefined => JsPrimitive::Undefined,
            ValueType::Null => JsPrimitive::Null,
            ValueType::Boolean(b) => JsPrimitive::Boolean(b),
            ValueType::Float => JsPrimitive::Float(value.float()),
            ValueType::FloatNaN => JsPrimitive::Float(value.float()),
            ValueType::String(str) => JsPrimitive::String(str),
            ValueType::Object(_)
            | ValueType::StringReference(_)
            | ValueType::NumberReference(_)
            | ValueType::Local(_) => {
                panic!("Cannot be wrapped")
            }
        }
    }
}
