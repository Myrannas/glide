use crate::debugging::{DebugRepresentation, DebugWithRealm, Renderer, Representation, Unwrap, X};
use crate::object_pool::ObjectPointer;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::string_pool::StringPointer;
use crate::values::symbols::JsSymbol;
use crate::{ExecutionError, InternalError, JsPrimitiveString, JsThread, Realm};
use instruction_set::Constant;
use std::marker::PhantomData;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Value<'a> {
    inner: u64,
    phantom: PhantomData<ObjectPointer<'a>>,
}

impl<'a> Value<'a> {
    pub(crate) fn iter<'b>(
        &self,
        realm: &'b Realm<'a>,
    ) -> JsResult<'a, impl Iterator<Item = &'b Value<'a>>> {
        if let Ok(obj) = self.as_object(realm) {
            let o = realm.objects.get(obj);

            Ok(o.indexed_properties.iter())
        } else {
            Err(ExecutionError::TypeError("Not iterable".to_string()))
        }
    }
}

#[derive(Copy, Clone)]
pub enum ValueType<'a> {
    Float,
    Object(ObjectPointer<'a>),
    String(JsPrimitiveString),
    Null,
    Undefined,
    Boolean(bool),
    Local(u32),
    StringReference(JsPrimitiveString),
    NumberReference(u32),
    SymbolReference(JsSymbol),
    Symbol(JsSymbol),
}

const DOWNSHIFT: u64 = 47;
const FLOAT_NAN_TAG: u64 = 9_221_120_237_041_090_560_u64 >> DOWNSHIFT;
const OBJECT_TAG: u64 = FLOAT_NAN_TAG + 1;
const NULL_TAG: u64 = FLOAT_NAN_TAG + 2;
const UNDEFINED_TAG: u64 = FLOAT_NAN_TAG + 3;
const STRING_TAG: u64 = FLOAT_NAN_TAG + 4;
const BOOLEAN_TRUE_TAG: u64 = FLOAT_NAN_TAG + 5;
const BOOLEAN_FALSE_TAG: u64 = FLOAT_NAN_TAG + 6;
const LOCAL_TAG: u64 = FLOAT_NAN_TAG + 7;
const STRING_REFERENCE_TAG: u64 = FLOAT_NAN_TAG + 8;
const NUMBER_REFERENCE_TAG: u64 = FLOAT_NAN_TAG + 9;
const SYMBOL_REFERENCE_TAG: u64 = FLOAT_NAN_TAG + 10;
const SYMBOL_TAG: u64 = FLOAT_NAN_TAG + 11;

impl<'a> From<ValueType<'a>> for Value<'a> {
    fn from(value: ValueType<'a>) -> Self {
        match value {
            ValueType::Object(object) => {
                let ptr: u32 = object.into();
                Value {
                    inner: (OBJECT_TAG << DOWNSHIFT) + u64::from(ptr),
                    phantom: PhantomData,
                }
            }
            ValueType::String(str) => {
                let str_index: u32 = str.into();

                Value {
                    inner: (STRING_TAG << DOWNSHIFT) + u64::from(str_index),
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
                let str_index: u32 = str.into();

                Value {
                    inner: (STRING_REFERENCE_TAG << DOWNSHIFT) + u64::from(str_index),
                    phantom: PhantomData,
                }
            }
            ValueType::NumberReference(number_reference) => Value {
                inner: (NUMBER_REFERENCE_TAG << DOWNSHIFT) + u64::from(number_reference),
                phantom: PhantomData,
            },
            ValueType::SymbolReference(symbol_reference) => Value {
                inner: (SYMBOL_REFERENCE_TAG << DOWNSHIFT) + u64::from(symbol_reference),
                phantom: PhantomData,
            },
            ValueType::Symbol(id) => Value {
                inner: (SYMBOL_TAG << DOWNSHIFT) + u64::from(id),
                phantom: PhantomData,
            },
            ValueType::Float => panic!("Unreachable"),
        }
    }
}

impl<'a> From<f64> for Value<'a> {
    fn from(value: f64) -> Self {
        if value.is_nan() {
            return Value::NAN;
        }

        Value {
            inner: value.to_bits(),
            phantom: PhantomData,
        }
    }
}

impl<'a> From<u32> for Value<'a> {
    fn from(value: u32) -> Self {
        Value::from(value as f64)
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
        inner: 0_u64,
        phantom: PhantomData,
    };
    pub const MINUS_ZERO: Self = Value {
        inner: 9223372036854775808_u64,
        phantom: PhantomData,
    };

    #[must_use]
    pub fn get_type(self) -> ValueType<'a> {
        let tag = self.inner >> DOWNSHIFT;

        match tag {
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
            SYMBOL_REFERENCE_TAG => ValueType::SymbolReference((self.inner as u32).into()),
            SYMBOL_TAG => ValueType::Symbol(JsSymbol::from(self.inner as u32)),
            _ => ValueType::Float,
        }
    }

    pub(crate) fn float(self) -> f64 {
        f64::from_bits(self.inner)
    }

    #[must_use]
    pub fn from_constant(atoms: &[JsPrimitiveString], constant: Constant) -> Value<'a> {
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

#[cfg(test)]
mod test {
    use crate::values::nan::{Value, ValueType};

    #[test]
    fn test() {
        assert_eq!(std::mem::size_of::<Value>(), 8);
        assert_eq!(std::mem::size_of::<ValueType>(), 8);
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
                render.string_literal(render.realm.strings.get(str).as_ref())
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
                render.string_literal(render.realm.strings.get(reference).as_ref())?;
                render.end_internal()?;
                Ok(())
            }
            (.., ValueType::Symbol(id)) => {
                render.literal("Symbol(")?;
                if let Some(name) = render.realm.symbols.get_name(id) {
                    render.string_literal(render.realm.strings.get(name).as_ref())?;
                }
                render.literal(")")?;
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
                let base_object = base.to_object(&mut thread.realm)?;

                base_object.get_value(thread, name)
            }
            ValueType::NumberReference(index) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                base_object.get_indexed(thread, index as usize)
            }
            ValueType::SymbolReference(symbol) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                base_object.get_value(thread, symbol)
            }
            _ => Ok(self),
        }
    }

    pub(crate) fn update_reference(
        self,
        thread: &mut JsThread<'a>,
        with_value: impl FnOnce(Value<'a>, &mut JsThread<'a>) -> JsResult<'a, Value<'a>>,
    ) -> JsResult<'a> {
        match self.get_type() {
            ValueType::Local(index) => {
                let value = thread.current_context().read(index as usize);

                let updated_value = with_value(value, thread)?;

                thread
                    .current_context()
                    .write(index as usize, updated_value);

                Ok(updated_value)
            }
            ValueType::StringReference(name) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                let original = base_object.get_value(thread, name)?;

                let updated_value = with_value(original, thread)?;

                base_object.set(thread, name, updated_value)?;
                Ok(updated_value)
            }
            ValueType::SymbolReference(symbol) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                let original = base_object.get_value(thread, symbol)?;

                let updated_value = with_value(original, thread)?;

                base_object.set(thread, symbol, updated_value)?;
                Ok(updated_value)
            }
            ValueType::NumberReference(index) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                let original = base_object.get_indexed(thread, index as usize)?;

                let updated_value = with_value(original, thread)?;

                base_object.set_indexed(thread, index as usize, updated_value)?;
                Ok(updated_value)
            }
            _ => InternalError::new_stackless(format!(
                "Unable to update - {}",
                thread.debug_value(&self)
            ))
            .into(),
        }
    }

    pub(crate) fn delete_reference(self, thread: &mut JsThread<'a>) -> JsResult<'a, ()> {
        match self.get_type() {
            ValueType::Local(index) => {
                thread
                    .current_context()
                    .write(index as usize, Value::UNDEFINED);

                thread.push_stack(true);

                Ok(())
            }
            ValueType::StringReference(name) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                let result = base_object.delete(&mut thread.realm.objects, name)?;

                thread.push_stack(result);

                Ok(())
            }
            ValueType::NumberReference(index) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                base_object.delete_indexed(&mut thread.realm.objects, index as usize);

                thread.push_stack(true);

                Ok(())
            }
            ValueType::SymbolReference(symbol) => {
                let base: Value = thread.pop_stack();
                let base = base.resolve(thread)?;
                let base_object = base.to_object(&mut thread.realm)?;

                let result = base_object.delete(&mut thread.realm.objects, symbol)?;

                thread.push_stack(result);

                Ok(())
            }
            _ => {
                thread.push_stack(true);
                Ok(())
            }
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
                if let Some(value) = obj.unwrap(&thread.realm.objects) {
                    return value.to_string(thread);
                }

                let to_string: Value = obj.get_value(thread, thread.realm.constants.to_string)?;

                // println!("{:?}\n{:?}", obj, to_string);

                if to_string == ValueType::Undefined.into() {
                    return Ok(thread.realm.strings.intern("[Object object]"));
                }

                if let ValueType::Object(function) = to_string.get_type() {
                    if let Some(callable) = function.get_callable(&thread.realm.objects) {
                        let callable = callable.clone();

                        let result = thread.call_from_native(obj.into(), callable, 0, false)?;

                        return result.to_string(thread);
                    }
                }

                return Err(thread
                    .new_type_error("Cannot convert object to primitive value")
                    .into());
            }
            ValueType::Symbol(symbol) => {
                if let Some(name) = thread.realm.symbols.get_name(symbol) {
                    let symbol_name = thread.realm.strings.intern(format!(
                        "Symbol({})",
                        thread.realm.strings.get(name).as_ref()
                    ));
                    return Ok(symbol_name);
                } else {
                    return Err(ExecutionError::InternalError(InternalError::new_stackless(
                        "Unable to locate name for symbol",
                    )));
                }
            }
            _ => todo!("Unsupported types {:?}", thread.debug_value(&self)),
        };

        Ok(result)
    }

    pub fn to_object(self, realm: &mut Realm<'a>) -> JsResult<'a, ObjectPointer<'a>> {
        let result = match self.get_type() {
            ValueType::String(str) => realm.wrappers.wrap_string(&mut realm.objects, str),
            ValueType::Object(obj) => obj,
            ValueType::Float => realm.wrappers.wrap_number(&mut realm.objects, self.float()),
            ValueType::Boolean(f) => realm.wrappers.wrap_boolean(&mut realm.objects, f),
            _ => {
                return Err(ExecutionError::TypeError(format!(
                    "Can't wrap {:?} with object",
                    X::from(&self, realm)
                )))
            }
        };

        Ok(result)
    }

    pub fn as_object(&self, realm: &Realm<'a>) -> JsResult<'a, ObjectPointer<'a>> {
        match self.get_type() {
            ValueType::Object(obj) => Ok(obj),
            _other => Err(ExecutionError::TypeError(format!(
                "{} is not a function",
                X::from(self, realm)
            ))),
        }
    }

    pub(crate) fn to_bool(self, realm: &Realm<'a>) -> bool {
        match self.get_type() {
            ValueType::Float => self.float() > 0.0,
            ValueType::Boolean(bool) => bool,
            ValueType::String(str) if str == realm.constants.undefined => false,
            ValueType::String(str) if str == realm.constants.empty_string => false,
            ValueType::String(..) | ValueType::Object(..) => true,
            ValueType::Null | ValueType::Undefined => false,
            _ => todo!("Unsupported types {:?}", realm.debug_value(&self)),
        }
    }

    pub(crate) fn to_number(self, realm: &Realm) -> JsResult<'a, f64> {
        match self.get_type() {
            ValueType::Undefined | ValueType::Object(..) => Ok(f64::NAN),
            ValueType::Null | ValueType::Boolean(false) => Ok(0.0),
            ValueType::Boolean(true) => Ok(1.0),
            ValueType::Float => Ok(self.float()),
            ValueType::StringReference(..)
            | ValueType::NumberReference(..)
            | ValueType::SymbolReference(..) => {
                todo!("References are not supported")
            }
            ValueType::String(value) => Ok(realm
                .strings
                .get(value)
                .as_ref()
                .parse()
                .unwrap_or(f64::NAN)),
            ValueType::Symbol(_) => Err(ExecutionError::TypeError(
                "Can't convert a Symbol value to a number".to_owned(),
            )),
            ValueType::Local(..) => panic!("Can't convert a local runtime value to a number"),
        }
    }

    pub(crate) fn to_usize(self, realm: &Realm) -> JsResult<'a, usize> {
        let value: f64 = self.to_number(realm)?;

        Ok(value.trunc() as usize)
    }

    pub(crate) fn to_u32(self, realm: &Realm) -> JsResult<'a, u32> {
        let input: f64 = self.to_number(realm)?;

        let f_val = match input {
            f if f.is_nan() => 0.0,
            f if f.is_infinite() => 0.0,
            input => input,
        };

        Ok(f_val.abs() as u32)
    }

    pub(crate) fn to_i32(self, realm: &Realm) -> JsResult<'a, i32> {
        let input: f64 = self.to_number(realm)?;

        let f_val = match input {
            f if f.is_nan() => 0.0,
            f if f.is_infinite() => 0.0,
            input => input,
        };

        Ok(f_val as i32)
    }

    pub fn call(self, thread: &mut JsThread<'a>, args: &[Value<'a>]) -> JsResult<'a, Value<'a>> {
        match self.get_type() {
            ValueType::Object(obj) => obj.call(thread, args),
            _ => Err(thread
                .new_type_error(format!("{} is not a function", thread.debug_value(&self)))
                .into()),
        }
    }

    pub(crate) fn strict_eq(self, other: Self) -> bool {
        let left = if self.inner == Self::MINUS_ZERO.inner {
            Self::ZERO.inner
        } else {
            self.inner
        };

        let right = if other.inner == Self::MINUS_ZERO.inner {
            Self::ZERO.inner
        } else {
            other.inner
        };

        left == right && self.inner != Self::NAN.inner
    }

    pub(crate) fn non_strict_eq(self, other: Self, frame: &mut JsThread<'a>) -> bool {
        if self.strict_eq(other) {
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
                let unwrapped_value = b1.unwrap(&frame.realm.objects);

                unwrapped_value.map_or(false, |b1| {
                    let string_value1: JsPrimitiveString =
                        b1.to_string(frame).unwrap_value(frame.get_realm());

                    s1.eq(&string_value1)
                })
            }
            _ => false,
        }
    }
}
