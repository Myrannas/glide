use std::fmt::{Debug, Display, Formatter};

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

#[derive(Clone, Debug)]
pub enum RuntimeValue<'a> {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(JsPrimitiveString),
    Object(ObjectPointer<'a>),
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
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1 == b2,
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => b1 == b2,
            (RuntimeValue::Null, RuntimeValue::Null)
            | (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            _ => false,
        }
    }

    pub(crate) fn non_strict_eq(&self, other: &Self, frame: &mut JsThread<'a>) -> bool {
        match (self, other) {
            (RuntimeValue::Undefined, RuntimeValue::Undefined) => true,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::String(b1), RuntimeValue::String(b2)) => b1.eq(b2),
            (RuntimeValue::String(b1), RuntimeValue::Float(f))
                if *b1 == frame.realm.constants.empty_string && *f as u32 == 0 =>
            {
                true
            }
            (RuntimeValue::String(b1), RuntimeValue::Boolean(false))
                if *b1 == frame.realm.constants.empty_string =>
            {
                true
            }
            (RuntimeValue::Float(b1), RuntimeValue::Float(b2)) => b1 == b2,
            (RuntimeValue::Object(b1), RuntimeValue::Object(b2)) => b1 == b2,
            (RuntimeValue::String(s1), RuntimeValue::Object(b1))
            | (RuntimeValue::Object(b1), RuntimeValue::String(s1)) => {
                let unwrapped_value = b1.unwrap(frame);

                unwrapped_value.map_or(false, |b1| {
                    let string_value1: JsPrimitiveString = b1.to_string(frame).unwrap();

                    s1.eq(&string_value1)
                })
            }
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            _ => false,
        }
    }

    pub fn call(&self, thread: &mut JsThread<'a>, args: &[RuntimeValue<'a>]) -> JsResult<'a> {
        match self {
            RuntimeValue::Object(obj) => obj.call(thread, args),
            other => Err(ExecutionError::Thrown(
                thread.new_type_error(format!("{} is not a function", other)),
                None,
            )),
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
        let result = match self.clone() {
            RuntimeValue::Internal(InternalValue::Local(index)) => {
                Ok(thread.current_context().read(index))
            }
            RuntimeValue::Reference(Reference::String { base, name, .. }) => {
                base.get_value(thread, name)
            }
            RuntimeValue::Reference(Reference::Number { base, name, .. }) => {
                base.get_indexed(thread, name)
            }
            other => Ok(other),
        }?;

        Ok(result)
    }

    pub(crate) fn update_reference(
        self,
        thread: &mut JsThread<'a>,
        value: impl Into<RuntimeValue<'a>>,
    ) -> JsResult<'a, ()> {
        match self {
            RuntimeValue::Internal(InternalValue::Local(index)) => {
                thread.current_context().write(index, value.into());
                Ok(())
            }
            RuntimeValue::Reference(Reference::String { base, name, .. }) => {
                base.set(&mut thread.realm.objects, name, value.into());
                Ok(())
            }
            RuntimeValue::Reference(Reference::Number { base, name, .. }) => {
                base.set_indexed(thread, name, value.into());
                Ok(())
            }
            value => InternalError::new_stackless(format!("Unable to update - {:?}", value)).into(),
        }
    }

    pub fn to_string(&self, thread: &mut JsThread<'a>) -> JsResult<'a, JsPrimitiveString> {
        let strings = &mut thread.realm.strings;

        let result: JsPrimitiveString = match self {
            RuntimeValue::Float(v) => strings.intern((*v).to_string()),
            RuntimeValue::Boolean(true) => thread.realm.constants.r#true,
            RuntimeValue::Boolean(false) => thread.realm.constants.r#false,
            RuntimeValue::String(str) => *str,
            RuntimeValue::Undefined => thread.realm.constants.undefined,
            RuntimeValue::Null => thread.realm.constants.null,
            RuntimeValue::Object(obj) => {
                let to_string = obj.get_value(thread, thread.realm.constants.to_string)?;

                // println!("{:?}\n{:?}", obj, to_string);

                if to_string == RuntimeValue::Undefined {
                    return Ok(thread.realm.strings.intern("[Object object]"));
                }

                if let RuntimeValue::Object(function) = to_string {
                    if let Some(callable) = function.get_callable(&mut thread.realm.objects) {
                        let callable = callable.clone();

                        return thread
                            .call_from_native((*obj).into(), callable, 0, false)?
                            .to_string(thread);
                    }
                }

                return Err(thread
                    .new_type_error("Cannot convert object to primitive value")
                    .into());
            }
            value => todo!("Unsupported types {:?}", value),
        };

        Ok(result)
    }

    pub(crate) fn to_object(&self, thread: &mut JsThread<'a>) -> JsResult<'a, ObjectPointer<'a>> {
        let result = match self {
            RuntimeValue::String(str) => thread
                .realm
                .wrappers
                .wrap_string(&mut thread.realm.objects, str.clone()),
            RuntimeValue::Object(obj) => *obj,
            RuntimeValue::Float(f) => thread
                .realm
                .wrappers
                .wrap_number(&mut thread.realm.objects, *f),
            RuntimeValue::Boolean(f) => thread
                .realm
                .wrappers
                .wrap_boolean(&mut thread.realm.objects, *f),
            // RuntimeValue::Und(_, obj) => obj.clone(),
            value => {
                return Err(thread
                    .new_type_error(format!("Can't wrap {} with object", value))
                    .into())
            }
        };

        Ok(result)
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
            RuntimeValue::Reference(..) => todo!("References are not supported"),
            RuntimeValue::String(value) => realm
                .strings
                .get(*value)
                .as_ref()
                .parse()
                .unwrap_or(f64::NAN),
            RuntimeValue::Internal(..) => panic!("Can't convert a local runtime value to a number"),
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
    arguments: Vec<RuntimeValue<'a>>,
    thread: &mut JsThread<'a>,
) -> RuntimeValue<'a> {
    thread
        .realm
        .wrappers
        .wrap_arguments(&mut thread.realm.objects, arguments)
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
            (.., RuntimeValue::Reference(reference)) => reference.render(render),
            (Representation::Debug, RuntimeValue::Internal(InternalValue::Local(local))) => {
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
