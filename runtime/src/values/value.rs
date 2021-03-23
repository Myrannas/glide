use std::fmt::{Debug, Display, Formatter};

use super::nan::Value;
use super::string::JsPrimitiveString;
use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::object_pool::ObjectPointer;
use crate::primordials::RuntimeHelpers;
use crate::result::ExecutionError;
use crate::result::JsResult;
use crate::string_pool::StringPool;
use crate::vm::JsThread;
use crate::{InternalError, Realm};
use instruction_set::Constant;

#[derive(Clone, Debug)]
pub enum Reference<'a> {
    String {
        base: ObjectPointer<'a>,
        name: JsPrimitiveString,
    },
    Number {
        base: ObjectPointer<'a>,
        name: usize,
    },
}

impl<'a> DebugRepresentation<'a> for Reference<'a> {
    fn render(&self, render: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        match (&render.representation, self) {
            (Representation::Debug, Reference::String { base, name }) => {
                render.start_internal("REF")?;
                ObjectPointer::render(base, render)?;

                render.formatter.write_fmt(format_args!(
                    "{}.",
                    // if *strict { "." } else { "?." },
                    &name,
                ))?;

                render.end_internal()?;
                Ok(())
            }
            (Representation::Debug, Reference::Number { base, name }) => {
                render.start_internal("REF")?;
                ObjectPointer::render(base, render)?;

                render.formatter.write_fmt(format_args!("{}.", &name,))?;

                render.end_internal()?;
                Ok(())
            }

            (Representation::Compact, Reference::String { base: _, name }) => {
                render.start_internal("REF")?;

                render.formatter.write_fmt(format_args!("{}.", &name,))?;

                render.end_internal()?;
                Ok(())
            }
            (Representation::Compact, Reference::Number { base: _, name }) => {
                render.start_internal("REF")?;

                render.formatter.write_fmt(format_args!("{}.", &name,))?;

                render.end_internal()?;
                Ok(())
            }
            _ => unimplemented!("Unsupported debug view"),
        }
    }
}

impl<'a> From<Option<RuntimeValue<'a>>> for RuntimeValue<'a> {
    fn from(value: Option<RuntimeValue<'a>>) -> Self {
        match value {
            Some(value) => value,
            None => RuntimeValue::Undefined,
        }
    }
}

#[deprecated]
#[derive(Clone, Debug, Copy)]
pub enum RuntimeValue<'a> {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(JsPrimitiveString),
    Object(ObjectPointer<'a>),
    StringReference(JsPrimitiveString),
    NumberReference(u32),
    Local(usize),
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
        let left: Value = (*self).into();
        let right: Value = (*other).into();

        left.strict_eq(right)
    }

    pub(crate) fn non_strict_eq(&self, other: &Self, frame: &mut JsThread<'a>) -> bool {
        let left: Value = (*self).into();
        let right: Value = (*other).into();

        left.non_strict_eq(right, frame)
    }

    pub fn call(&self, thread: &mut JsThread<'a>, args: &[Value<'a>]) -> JsResult<'a, Value<'a>> {
        match self {
            RuntimeValue::Object(obj) => obj.call(thread, args),
            other => Err(thread
                .new_type_error(format!("{} is not a function", other))
                .into()),
        }
    }

    pub fn from_constant(atoms: &Vec<JsPrimitiveString>, constant: &Constant) -> RuntimeValue<'a> {
        match constant {
            Constant::Null => RuntimeValue::Null,
            Constant::Undefined => RuntimeValue::Undefined,
            Constant::Float(value) => RuntimeValue::Float(*value),
            Constant::Boolean(value) => RuntimeValue::Boolean(*value),
            Constant::Atom(value) => RuntimeValue::String(atoms[*value]),
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
        let value: Value<'a> = self.into();

        value.resolve(thread).map(RuntimeValue::from)
    }

    pub(crate) fn update_reference(
        self,
        thread: &mut JsThread<'a>,
        with_value: impl FnOnce(RuntimeValue<'a>, &mut JsThread<'a>) -> JsResult<'a>,
    ) -> JsResult<'a, ()> {
        let value: Value<'a> = self.into();

        value.update_reference(thread, |v, t| with_value(v.into(), t).map(Value::from))
    }

    pub fn to_string(&self, thread: &mut JsThread<'a>) -> JsResult<'a, JsPrimitiveString> {
        let value: Value<'a> = (*self).into();

        value.to_string(thread)
    }

    pub(crate) fn to_object(&self, thread: &mut JsThread<'a>) -> JsResult<'a, ObjectPointer<'a>> {
        let value: Value<'a> = (*self).into();

        value.to_object(thread)
    }

    pub(crate) fn to_bool(&self, realm: &Realm) -> bool {
        match self {
            RuntimeValue::Float(v) => *v > 0.0,
            RuntimeValue::Boolean(bool) => *bool,
            RuntimeValue::String(str) if *str == realm.constants.undefined => false,
            RuntimeValue::String(str) if *str == realm.constants.empty_string => false,
            RuntimeValue::String(..) | RuntimeValue::Object(..) => true,
            RuntimeValue::Null | RuntimeValue::Undefined => false,
            value => todo!("Unsupported types {:?}", value),
        }
    }

    pub(crate) fn to_number(&self, realm: &Realm) -> f64 {
        match self {
            RuntimeValue::Undefined | RuntimeValue::Object(..) => f64::NAN,
            RuntimeValue::Null | RuntimeValue::Boolean(false) => 0.0,
            RuntimeValue::Boolean(true) => 1.0,
            RuntimeValue::Float(v) => *v,
            RuntimeValue::StringReference(..) | RuntimeValue::NumberReference(..) => {
                todo!("References are not supported")
            }
            RuntimeValue::String(value) => realm
                .strings
                .get(*value)
                .as_ref()
                .parse()
                .unwrap_or(f64::NAN),
            RuntimeValue::Local(..) => panic!("Can't convert a local runtime value to a number"),
        }
    }

    pub(crate) fn to_usize(&self, realm: &Realm) -> usize {
        let value: f64 = self.to_number(realm);

        value.trunc() as usize
    }

    pub(crate) fn to_u32(&self, realm: &Realm) -> u32 {
        let input: f64 = self.to_number(realm);

        let f_val = match input {
            f if f.is_nan() => 0.0,
            f if f.is_infinite() => 0.0,
            input => input,
        };

        f_val.abs() as u32
    }

    pub(crate) fn to_i32(&self, realm: &Realm) -> i32 {
        let input: f64 = self.to_number(realm);

        let f_val = match input {
            f if f.is_nan() => 0.0,
            f if f.is_infinite() => 0.0,
            input => input,
        };

        f_val as i32
    }
}

impl<'a> Display for RuntimeValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

pub(crate) fn make_arguments<'a>(
    arguments: Vec<Value<'a>>,
    thread: &mut JsThread<'a>,
) -> Value<'a> {
    thread
        .realm
        .wrappers
        .wrap_arguments(
            &mut thread.realm.objects,
            arguments.into_iter().map(From::from).collect(),
        )
        .into()
}

#[derive(Clone, Debug)]
pub enum InternalValue {
    Local(usize),
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

impl<'a> From<i32> for RuntimeValue<'a> {
    #[allow(clippy::cast_lossless)]
    fn from(value: i32) -> Self {
        f64::from(value).into()
    }
}

impl<'a> From<ObjectPointer<'a>> for RuntimeValue<'a> {
    fn from(value: ObjectPointer<'a>) -> Self {
        RuntimeValue::Object(value)
    }
}
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

impl<'a, 'b> DebugRepresentation<'a> for RuntimeValue<'a> {
    fn render(&self, render: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        match (&render.representation, self) {
            (.., RuntimeValue::Boolean(true)) => render.literal("true"),
            (.., RuntimeValue::Boolean(false)) => render.literal("false"),
            (.., RuntimeValue::Undefined) => render.literal("undefined"),
            (.., RuntimeValue::Null) => render.literal("null"),
            (.., RuntimeValue::Object(obj)) => render.render(obj),
            (.., RuntimeValue::String(str)) => {
                render.string_literal(render.thread.realm.strings.get(*str).as_ref())
            }
            (.., RuntimeValue::NumberReference(reference)) => {
                render.start_internal("REFERENCE")?;
                render.internal_key("index")?;
                render.literal(&reference.to_string())?;
                render.end_internal()?;
                Ok(())
            }
            (.., RuntimeValue::StringReference(reference)) => {
                render.start_internal("REFERENCE")?;
                render.internal_key("key")?;
                render.string_literal(render.thread.realm.strings.get(*reference).as_ref())?;
                render.end_internal()?;
                Ok(())
            }
            (Representation::Debug, RuntimeValue::Local(local)) => {
                render.start_internal("INTERNAL")?;
                render.internal_key("index")?;
                render.literal(&local.to_string())?;
                render.end_internal()?;
                Ok(())
            }
            (.., RuntimeValue::Float(value)) => render.literal(&format!("{}", value)),
            _ => panic!("Unsupported debug view"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::RuntimeValue;

    #[test]
    fn check_runtime_value_size() {
        assert_eq!(std::mem::size_of::<RuntimeValue>(), 8);
    }
}
