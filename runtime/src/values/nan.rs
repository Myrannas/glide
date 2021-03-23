use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::object_pool::ObjectPointer;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::string_pool::StringPointer;
use crate::{ExecutionError, InternalError, JsPrimitiveString, JsThread, Realm, RuntimeValue};
use instruction_set::Constant;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Value<'a> {
    inner: u64,
    phantom: PhantomData<ObjectPointer<'a>>,
}

impl<'a> Debug for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tpe = self.get_type();

        match tpe {
            ValueType::Float => f.write_fmt(format_args!("{}", self.float())),
            tpe => f.write_fmt(format_args!("{:?}", tpe)),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum ValueType<'a> {
    Float,
    FloatNaN,
    Object(ObjectPointer<'a>),
    String(JsPrimitiveString),
    Null,
    Undefined,
    Boolean(bool),
    Local(u32),
    StringReference(JsPrimitiveString),
    NumberReference(u32),
}

const DOWNSHIFT: u64 = 47;
const FLOAT_NAN_TAG: u64 = 9221120237041090560u64 >> DOWNSHIFT;
const OBJECT_TAG: u64 = FLOAT_NAN_TAG + 1;
const NULL_TAG: u64 = FLOAT_NAN_TAG + 2;
const UNDEFINED_TAG: u64 = FLOAT_NAN_TAG + 3;
const STRING_TAG: u64 = FLOAT_NAN_TAG + 4;
const BOOLEAN_TRUE_TAG: u64 = FLOAT_NAN_TAG + 5;
const BOOLEAN_FALSE_TAG: u64 = FLOAT_NAN_TAG + 6;
const LOCAL_TAG: u64 = FLOAT_NAN_TAG + 7;
const STRING_REFERENCE_TAG: u64 = FLOAT_NAN_TAG + 8;
const NUMBER_REFERENCE_TAG: u64 = FLOAT_NAN_TAG + 9;

impl<'a> From<ValueType<'a>> for Value<'a> {
    fn from(value: ValueType<'a>) -> Self {
        match value {
            ValueType::FloatNaN => Value {
                inner: f64::NAN.to_bits(),
                phantom: PhantomData,
            },
            ValueType::Object(object) => {
                let ptr: u32 = object.into();
                Value {
                    inner: (OBJECT_TAG << DOWNSHIFT) + u64::from(ptr),
                    phantom: PhantomData,
                }
            }
            ValueType::String(str) => {
                let ptr: u32 = str.into();

                Value {
                    inner: (STRING_TAG << DOWNSHIFT) + u64::from(ptr),
                    phantom: PhantomData,
                }
            }
            ValueType::Null => Value {
                inner: (NULL_TAG << DOWNSHIFT),
                phantom: PhantomData,
            },
            ValueType::Undefined => Value {
                inner: (UNDEFINED_TAG << DOWNSHIFT),
                phantom: PhantomData,
            },
            ValueType::Boolean(true) => Value {
                inner: (BOOLEAN_TRUE_TAG << DOWNSHIFT),
                phantom: PhantomData,
            },
            ValueType::Boolean(false) => Value {
                inner: (BOOLEAN_FALSE_TAG << DOWNSHIFT),
                phantom: PhantomData,
            },
            ValueType::Local(local) => Value {
                inner: (LOCAL_TAG << DOWNSHIFT) + u64::from(local),
                phantom: PhantomData,
            },
            ValueType::StringReference(str) => {
                let ptr: u32 = str.into();

                Value {
                    inner: (STRING_REFERENCE_TAG << DOWNSHIFT) + u64::from(ptr),
                    phantom: PhantomData,
                }
            }
            ValueType::NumberReference(number_reference) => Value {
                inner: (STRING_REFERENCE_TAG << DOWNSHIFT) + u64::from(number_reference),
                phantom: PhantomData,
            },
            ValueType::Float => panic!("Unreachable"),
        }
    }
}

impl<'a> From<RuntimeValue<'a>> for Value<'a> {
    fn from(value: RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Undefined => ValueType::Undefined.into(),
            RuntimeValue::Null => ValueType::Null.into(),
            RuntimeValue::Boolean(value) => ValueType::Boolean(value).into(),
            RuntimeValue::Float(float) => float.into(),
            RuntimeValue::String(str) => ValueType::String(str).into(),
            RuntimeValue::Object(obj) => ValueType::Object(obj).into(),
            RuntimeValue::StringReference(str) => ValueType::StringReference(str).into(),
            RuntimeValue::NumberReference(number) => ValueType::NumberReference(number).into(),
            RuntimeValue::Local(local) => ValueType::Local(local as u32).into(),
        }
    }
}

impl<'a> From<Value<'a>> for RuntimeValue<'a> {
    fn from(value: Value<'a>) -> Self {
        match value.get_type() {
            ValueType::Undefined => RuntimeValue::Undefined,
            ValueType::Null => RuntimeValue::Null,
            ValueType::Boolean(value) => RuntimeValue::Boolean(value),
            ValueType::Float => RuntimeValue::Float(value.float()),
            ValueType::String(str) => RuntimeValue::String(str),
            ValueType::Object(obj) => RuntimeValue::Object(obj),
            ValueType::StringReference(str) => RuntimeValue::StringReference(str),
            ValueType::NumberReference(number) => RuntimeValue::NumberReference(number),
            ValueType::Local(local) => RuntimeValue::Local(local as usize),
            ValueType::FloatNaN => RuntimeValue::Float(f64::NAN),
        }
    }
}

impl<'a> From<f64> for Value<'a> {
    fn from(value: f64) -> Self {
        Value {
            inner: value.to_bits(),
            phantom: PhantomData,
        }
    }
}

impl<'a> From<Option<Value<'a>>> for Value<'a> {
    fn from(value: Option<Value<'a>>) -> Self {
        value.unwrap_or_default()
    }
}

impl<'a> Default for Value<'a> {
    fn default() -> Self {
        ValueType::Undefined.into()
    }
}

impl<'a> Value<'a> {
    pub const UNDEFINED: Self = Value {
        inner: UNDEFINED_TAG << DOWNSHIFT,
        phantom: PhantomData,
    };
    pub const NAN: Self = Value {
        inner: FLOAT_NAN_TAG << DOWNSHIFT,
        phantom: PhantomData,
    };
    pub const NULL: Self = Value {
        inner: NULL_TAG << DOWNSHIFT,
        phantom: PhantomData,
    };
    pub const TRUE: Self = Value {
        inner: BOOLEAN_TRUE_TAG << DOWNSHIFT,
        phantom: PhantomData,
    };
    pub const FALSE: Self = Value {
        inner: BOOLEAN_FALSE_TAG << DOWNSHIFT,
        phantom: PhantomData,
    };
    pub const ZERO: Self = Value {
        inner: 0u64,
        phantom: PhantomData,
    };

    pub fn get_type(self) -> ValueType<'a> {
        let tag = self.inner >> DOWNSHIFT;

        match tag {
            FLOAT_NAN_TAG => ValueType::FloatNaN,
            OBJECT_TAG => ValueType::Object(ObjectPointer::new(self.inner as u32)),
            NULL_TAG => ValueType::Null,
            UNDEFINED_TAG => ValueType::Undefined,
            STRING_TAG => ValueType::String(StringPointer::new(self.inner as u32)),
            BOOLEAN_TRUE_TAG => ValueType::Boolean(true),
            BOOLEAN_FALSE_TAG => ValueType::Boolean(false),
            LOCAL_TAG => ValueType::Local(self.inner as u32),
            STRING_REFERENCE_TAG => {
                ValueType::StringReference(StringPointer::new(self.inner as u32))
            }
            NUMBER_REFERENCE_TAG => ValueType::NumberReference(self.inner as u32),
            _ => ValueType::Float,
        }
    }

    pub(crate) fn float(self) -> f64 {
        f64::from_bits(self.inner)
    }

    pub fn from_constant(atoms: &Vec<JsPrimitiveString>, constant: Constant) -> Value<'a> {
        match constant {
            Constant::Null => Self::NULL,
            Constant::Undefined => Self::UNDEFINED,
            Constant::Float(value) => Self::from(value),
            Constant::Boolean(value) => ValueType::Boolean(value).into(),
            Constant::Atom(value) => ValueType::String(atoms[value]).into(),
        }
    }

    pub(crate) fn is_local(self) -> bool {
        let tag = self.inner >> DOWNSHIFT;

        tag == LOCAL_TAG
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(value: bool) -> Self {
        ValueType::Boolean(value).into()
    }
}

impl<'a> From<ObjectPointer<'a>> for Value<'a> {
    fn from(value: ObjectPointer<'a>) -> Self {
        ValueType::Object(value).into()
    }
}

impl<'a> From<StringPointer> for Value<'a> {
    fn from(value: StringPointer) -> Self {
        ValueType::String(value).into()
    }
}

impl<'a> From<i32> for Value<'a> {
    #[allow(clippy::cast_lossless)]
    fn from(value: i32) -> Self {
        f64::from(value).into()
    }
}

impl<'a> From<Option<RuntimeValue<'a>>> for Value<'a> {
    #[allow(clippy::cast_lossless)]
    fn from(value: Option<RuntimeValue<'a>>) -> Self {
        value.unwrap_or_default().into()
    }
}

#[cfg(test)]
mod test {
    use crate::object_pool::ObjectPointer;
    use crate::values::nan::{Value, ValueType};
    use crate::RuntimeValue;

    #[test]
    fn test() {
        assert_eq!(std::mem::size_of::<Value>(), 8);
        assert_eq!(std::mem::size_of::<ValueType>(), 8);
    }

    #[test]
    fn convert_object() {
        let value = RuntimeValue::Object(ObjectPointer::new(1));
        let converted_value: Value = value.into();
        let converted_back: RuntimeValue = converted_value.into();

        assert_eq!(value, converted_back);
    }
}

impl<'a, 'b> DebugRepresentation<'a> for Value<'a> {
    fn render(&self, render: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        match (render.representation, self.get_type()) {
            (.., ValueType::Boolean(true)) => render.literal("true"),
            (.., ValueType::Boolean(false)) => render.literal("false"),
            (.., ValueType::Undefined) => render.literal("undefined"),
            (.., ValueType::Null) => render.literal("null"),
            (.., ValueType::Object(obj)) => render.render(&obj),
            (.., ValueType::String(str)) => {
                render.string_literal(render.thread.realm.strings.get(str).as_ref())
            }
            (.., ValueType::NumberReference(reference)) => {
                render.start_internal("REFERENCE")?;
                render.internal_key("index")?;
                render.literal(&reference.to_string())?;
                render.end_internal()?;
                Ok(())
            }
            (.., ValueType::StringReference(reference)) => {
                render.start_internal("REFERENCE")?;
                render.internal_key("key")?;
                render.string_literal(render.thread.realm.strings.get(reference).as_ref())?;
                render.end_internal()?;
                Ok(())
            }
            (Representation::Debug, ValueType::Local(local)) => {
                render.start_internal("INTERNAL")?;
                render.internal_key("index")?;
                render.literal(&local.to_string())?;
                render.end_internal()?;
                Ok(())
            }
            (.., ValueType::Float) => render.literal(&format!("{}", self.float())),
            _ => panic!("Unsupported debug view"),
        }
    }
}

impl<'a> Value<'a> {
    pub(crate) fn resolve<'c>(
        self,
        thread: &'c mut JsThread<'a>,
    ) -> Result<Self, ExecutionError<'a>> {
        match self.get_type() {
            ValueType::Local(index) => Ok(thread.current_context().read(index as usize)),
            ValueType::StringReference(name) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(thread)?;

                base_object.get_value(thread, name)
            }
            ValueType::NumberReference(index) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(thread)?;

                base_object.get_indexed(thread, index as usize)
            }
            _ => Ok(self),
        }
    }

    pub(crate) fn update_reference(
        self,
        thread: &mut JsThread<'a>,
        with_value: impl FnOnce(Value<'a>, &mut JsThread<'a>) -> JsResult<'a, Value<'a>>,
    ) -> JsResult<'a, ()> {
        match self.get_type() {
            ValueType::Local(index) => {
                let value = thread.current_context().read(index as usize);

                let updated_value = with_value(value, thread)?;

                thread
                    .current_context()
                    .write(index as usize, updated_value);

                Ok(())
            }
            ValueType::StringReference(name) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(thread)?;

                let original = base_object.get_value(thread, name)?;

                let updated_value = with_value(original, thread)?;

                base_object.set(&mut thread.realm.objects, name, updated_value);
                Ok(())
            }
            ValueType::NumberReference(index) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(thread)?;

                let original = base_object.get_indexed(thread, index as usize)?;

                let updated_value = with_value(original, thread)?;

                base_object.set_indexed(thread, index as usize, updated_value);
                Ok(())
            }
            value => InternalError::new_stackless(format!("Unable to update - {:?}", value)).into(),
        }
    }

    pub fn to_string(self, thread: &mut JsThread<'a>) -> JsResult<'a, JsPrimitiveString> {
        let strings = &mut thread.realm.strings;

        let result: JsPrimitiveString = match self.get_type() {
            ValueType::Float => strings.intern((self.float()).to_string()),
            ValueType::Boolean(true) => thread.realm.constants.r#true,
            ValueType::Boolean(false) => thread.realm.constants.r#false,
            ValueType::String(str) => str,
            ValueType::Undefined => thread.realm.constants.undefined,
            ValueType::Null => thread.realm.constants.null,
            ValueType::Object(obj) => {
                let to_string: Value = obj.get_value(thread, thread.realm.constants.to_string)?;

                // println!("{:?}\n{:?}", obj, to_string);

                if to_string == ValueType::Undefined.into() {
                    return Ok(thread.realm.strings.intern("[Object object]"));
                }

                if let ValueType::Object(function) = to_string.get_type() {
                    if let Some(callable) = function.get_callable(&mut thread.realm.objects) {
                        let callable = callable.clone();

                        let result = thread.call_from_native(obj.into(), callable, 0, false)?;

                        return result.to_string(thread);
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

    pub(crate) fn to_object(self, thread: &mut JsThread<'a>) -> JsResult<'a, ObjectPointer<'a>> {
        let result = match self.get_type() {
            ValueType::String(str) => thread
                .realm
                .wrappers
                .wrap_string(&mut thread.realm.objects, str),
            ValueType::Object(obj) => obj,
            ValueType::Float => thread
                .realm
                .wrappers
                .wrap_number(&mut thread.realm.objects, self.float()),
            ValueType::Boolean(f) => thread
                .realm
                .wrappers
                .wrap_boolean(&mut thread.realm.objects, f),
            value => {
                return Err(thread
                    .new_type_error(format!("Can't wrap {:?} with object", value))
                    .into())
            }
        };

        Ok(result)
    }

    pub(crate) fn to_bool(self, realm: &Realm) -> bool {
        match self.get_type() {
            ValueType::Float => self.float() > 0.0,
            ValueType::Boolean(bool) => bool,
            ValueType::String(str) if str == realm.constants.undefined => false,
            ValueType::String(str) if str == realm.constants.empty_string => false,
            ValueType::String(..) | ValueType::Object(..) => true,
            ValueType::Null | ValueType::Undefined => false,
            value => todo!("Unsupported types {:?}", value),
        }
    }

    pub(crate) fn to_number(self, realm: &Realm) -> f64 {
        match self.get_type() {
            ValueType::Undefined | ValueType::Object(..) => f64::NAN,
            ValueType::Null | ValueType::Boolean(false) => 0.0,
            ValueType::Boolean(true) => 1.0,
            ValueType::Float => self.float(),
            ValueType::FloatNaN => self.float(),
            ValueType::StringReference(..) | ValueType::NumberReference(..) => {
                todo!("References are not supported")
            }
            ValueType::String(value) => realm
                .strings
                .get(value)
                .as_ref()
                .parse()
                .unwrap_or(f64::NAN),
            ValueType::Local(..) => panic!("Can't convert a local runtime value to a number"),
        }
    }

    pub(crate) fn to_usize(self, realm: &Realm) -> usize {
        let value: f64 = self.to_number(realm);

        value.trunc() as usize
    }

    pub(crate) fn to_u32(self, realm: &Realm) -> u32 {
        let input: f64 = self.to_number(realm);

        let f_val = match input {
            f if f.is_nan() => 0.0,
            f if f.is_infinite() => 0.0,
            input => input,
        };

        f_val.abs() as u32
    }

    pub(crate) fn to_i32(self, realm: &Realm) -> i32 {
        let input: f64 = self.to_number(realm);

        let f_val = match input {
            f if f.is_nan() => 0.0,
            f if f.is_infinite() => 0.0,
            input => input,
        };

        f_val as i32
    }

    pub(crate) fn strict_eq(self, other: Self) -> bool {
        self.inner == other.inner
    }

    pub(crate) fn non_strict_eq(self, other: Self, frame: &mut JsThread<'a>) -> bool {
        if self.inner == other.inner {
            return true;
        }

        match (self.get_type(), other.get_type()) {
            (ValueType::String(b1), ValueType::Float)
                if b1 == frame.realm.constants.empty_string && self.float() as u32 == 0 =>
            {
                true
            }
            (ValueType::String(b1), ValueType::Boolean(false))
                if b1 == frame.realm.constants.empty_string =>
            {
                true
            }
            (ValueType::String(s1), ValueType::Object(b1))
            | (ValueType::Object(b1), ValueType::String(s1)) => {
                let unwrapped_value = b1.unwrap(frame);

                unwrapped_value.map_or(false, |b1| {
                    let string_value1: JsPrimitiveString = b1.to_string(frame).unwrap();

                    s1.eq(&string_value1)
                })
            }
            _ => false,
        }
    }
}
