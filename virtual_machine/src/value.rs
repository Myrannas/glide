use std::fmt::{Debug, Display, Formatter};

use crate::debugging::Renderer;
use crate::object::JsObject;
use crate::result::ExecutionError;
use crate::result::JsResult;
use crate::string::JsPrimitiveString;
use crate::vm::JsThread;
use crate::InternalError;
use instruction_set::Constant;

#[derive(Clone, Debug)]
pub struct Reference<'a> {
    pub(crate) base: Box<JsObject<'a>>,
    pub(crate) name: JsPrimitiveString,
    pub(crate) strict: bool,
}

impl<'a> From<Option<RuntimeValue<'a>>> for RuntimeValue<'a> {
    fn from(value: Option<RuntimeValue<'a>>) -> Self {
        match value {
            Some(value) => value,
            None => RuntimeValue::Undefined,
        }
    }
}

#[derive(Clone)]
pub enum RuntimeValue<'a> {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(JsPrimitiveString),
    Object(JsObject<'a>),
    Reference(Reference<'a>),
    Internal(InternalValue),
}

impl<'a> Default for RuntimeValue<'a> {
    fn default() -> Self {
        RuntimeValue::Undefined
    }
}

impl<'a> Default for &RuntimeValue<'a> {
    fn default() -> Self {
        &RuntimeValue::Undefined
    }
}

impl<'a> RuntimeValue<'a> {
    pub(crate) fn strict_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1.as_ref() == b2.as_ref(),
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => b1 == b2,
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }

    pub(crate) fn non_strict_eq(&self, other: &Self, frame: &mut JsThread<'a>) -> bool {
        match (self, other) {
            (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1.eq(b2),
            (RuntimeValue::String(b1), RuntimeValue::Float(f))
                if b1.as_ref() == "" && *f as u32 == 0 =>
            {
                true
            }
            (RuntimeValue::String(b1), RuntimeValue::Boolean(false)) if b1.as_ref() == "" => true,
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => b1 == b2,
            (RuntimeValue::String(s1), RuntimeValue::Object(b1)) => {
                let string_value1: JsPrimitiveString =
                    b1.get_wrapped_value().unwrap().to_string(frame).unwrap();

                s1.eq(&string_value1)
            }
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }

    pub(crate) fn as_object(&self) -> JsResult<'a, &JsObject<'a>> {
        match self {
            RuntimeValue::Object(obj) => Ok(obj),
            _ => InternalError::new_stackless("Unable to use as object").into(),
        }
    }
}

impl<'a> PartialEq for RuntimeValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.strict_eq(other)
    }
}

impl<'a> RuntimeValue<'a> {
    pub(crate) fn resolve<'c>(
        self,
        thread: &'c mut JsThread<'a>,
    ) -> Result<Self, ExecutionError<'a>> {
        let result = match self.clone() {
            RuntimeValue::Internal(InternalValue::Local(index)) => {
                Ok(thread.current_context().read(index))
            }
            RuntimeValue::Reference(reference) => reference.base.get(reference.name, thread),
            other => Ok(other),
        }?;

        Ok(result)
    }

    pub(crate) fn update_reference(
        self,
        frame: &mut JsThread<'a>,
        value: impl Into<RuntimeValue<'a>>,
    ) -> JsResult<'a, ()> {
        match self {
            RuntimeValue::Internal(InternalValue::Local(index)) => {
                frame.current_context().write(index, value.into());
                Ok(())
            }
            RuntimeValue::Reference(reference) => {
                reference.base.set(reference.name, value);
                Ok(())
            }
            value => InternalError::new_stackless(format!("Unable to update - {:?}", value)).into(),
        }
    }

    pub(crate) fn to_string(&self, thread: &mut JsThread<'a>) -> JsResult<'a, JsPrimitiveString> {
        let result: JsPrimitiveString = match self {
            RuntimeValue::Float(v) => v.to_string().into(),
            RuntimeValue::Boolean(bool) => bool.to_string().into(),
            RuntimeValue::String(str) => str.as_ref().into(),
            RuntimeValue::Undefined => "undefined".into(),
            RuntimeValue::Null => "null".into(),
            RuntimeValue::Object(obj) => {
                let to_string = obj.get("toString".into(), thread)?;

                // println!("{:?}\n{:?}", obj, to_string);

                match to_string {
                    RuntimeValue::Object(function) if function.is_callable() => thread
                        .call_from_native(obj.clone(), function.get_callable().unwrap(), 0, false)?
                        .to_string(thread)?,
                    RuntimeValue::Undefined => "[Object object]".to_owned().into(),
                    _ => {
                        let error = thread
                            .global_this
                            .errors
                            .new_type_error("Cannot convert object to primitive value");

                        return Err(error.into());
                    }
                }
            }
            value => todo!("Unsupported types {:?}", value),
        };

        Ok(result)
    }

    pub(crate) fn to_object(&self, thread: &mut JsThread<'a>) -> JsObject<'a> {
        match self {
            RuntimeValue::String(str) => thread.global_this.wrappers.wrap_string(str.clone()),
            RuntimeValue::Object(obj) => obj.clone(),
            // RuntimeValue::Und(_, obj) => obj.clone(),
            _ => panic!(":( {:?}", self),
        }
    }
}

impl<'a> Debug for RuntimeValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::debug(f, 5).render(self)
    }
}

impl<'a> Display for RuntimeValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Renderer::compact(f).render(self)
    }
}

pub(crate) fn make_arguments(arguments: Vec<RuntimeValue>) -> RuntimeValue {
    JsObject::new()
        .with_name("[Arguments]")
        .with_indexed_properties(arguments)
        .into()
}

#[derive(Clone, Debug)]
pub enum InternalValue {
    Local(usize),
    Branch(usize, usize),
    Jump(usize),
}

impl<'a> From<RuntimeValue<'a>> for f64 {
    fn from(value: RuntimeValue) -> Self {
        (&value).into()
    }
}

impl<'a> From<&RuntimeValue<'a>> for f64 {
    fn from(value: &RuntimeValue) -> Self {
        match value {
            RuntimeValue::Undefined => f64::NAN,
            RuntimeValue::Null => 0.0,
            RuntimeValue::Boolean(true) => 1.0,
            RuntimeValue::Boolean(false) => 0.0,
            RuntimeValue::Float(v) => *v,
            RuntimeValue::Reference(..) => todo!("References are not supported"),
            RuntimeValue::Object(..) => f64::NAN,
            RuntimeValue::String(value) => value.as_ref().parse().unwrap_or(f64::NAN),
            RuntimeValue::Internal(..) => panic!("Can't convert a local runtime value to a number"),
        }
    }
}

impl<'a> From<f64> for RuntimeValue<'a> {
    fn from(value: f64) -> Self {
        RuntimeValue::Float(value)
    }
}

impl<'a> From<bool> for RuntimeValue<'a> {
    fn from(value: bool) -> Self {
        RuntimeValue::Boolean(value)
    }
}

impl<'a> From<RuntimeValue<'a>> for bool {
    fn from(value: RuntimeValue<'a>) -> Self {
        (&value).into()
    }
}

impl<'a> From<String> for RuntimeValue<'a> {
    fn from(value: String) -> Self {
        RuntimeValue::String(value.into())
    }
}

impl<'a> From<&RuntimeValue<'a>> for bool {
    fn from(value: &RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Float(v) => *v > 0.0,
            RuntimeValue::Boolean(bool) => *bool,
            RuntimeValue::String(str) if str.as_ref().eq("undefined") => false,
            RuntimeValue::String(str) if str.as_ref().eq("") => false,
            RuntimeValue::String(..) => true,
            RuntimeValue::Undefined => false,
            RuntimeValue::Null => false,
            RuntimeValue::Object(..) => true,
            value => todo!("Unsupported types {:?}", value),
        }
    }
}

impl<'a> From<&Constant> for RuntimeValue<'a> {
    fn from(constant: &Constant) -> Self {
        match constant {
            Constant::Null => RuntimeValue::Null,
            Constant::Undefined => RuntimeValue::Undefined,
            Constant::Float(value) => RuntimeValue::Float(*value),
            Constant::Boolean(value) => RuntimeValue::Boolean(*value),
            Constant::String(value) => RuntimeValue::String(value.into()),
        }
    }
}

impl<'a> From<RuntimeValue<'a>> for u32 {
    fn from(input: RuntimeValue<'a>) -> Self {
        let input: f64 = input.into();

        let f_val = match input {
            f64::NAN => 0.0,
            f64::INFINITY => 0.0,
            input => input,
        };

        f_val.abs() as u32
    }
}

impl<'a> From<RuntimeValue<'a>> for i32 {
    fn from(input: RuntimeValue<'a>) -> Self {
        let input: f64 = input.into();

        let f_val = match input {
            f64::NAN => 0.0,
            f64::INFINITY => 0.0,
            input => input,
        };

        f_val as i32
    }
}

// impl<'a> From<RuntimeValue<'a>> for String {
//     fn from(value: RuntimeValue<'a>) -> Self {
//         (&value).into()
//     }
// }
//
// impl<'a> From<&RuntimeValue<'a>> for String {
//     fn from(value: &RuntimeValue<'a>) -> Self {
//         match value {
//             RuntimeValue::Float(v) => v.to_string(),
//             RuntimeValue::Boolean(bool) => bool.to_string(),
//             RuntimeValue::String(str, ..) => str.as_ref().to_owned(),
//             RuntimeValue::Undefined => "undefined".to_owned(),
//             RuntimeValue::Null => "null".to_owned(),
//             RuntimeValue::Object(_) => "[object Object]".to_owned(),
//             value => todo!("Unsupported types {:?}", value),
//         }
//     }
// }