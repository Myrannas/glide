use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Pointer, Write};
use std::hash::{BuildHasher, Hasher};

use colored::Colorize;

use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::values::primitives::JsPrimitive;

use super::string::JsPrimitiveString;
use super::value::RuntimeValue;
use crate::object_pool::{ObjectPointer, ObjectPool};
use crate::string_pool::StringPointer;
use crate::values::function::FunctionReference;
use ahash::AHashMap;

#[derive(Clone)]
pub struct JsObject<'a> {
    pub(crate) properties: HashMap<JsPrimitiveString, Property<'a>, PropertyHasher>,
    pub(crate) indexed_properties: Vec<RuntimeValue<'a>>,
    pub(crate) name: Option<JsPrimitiveString>,
    pub(crate) prototype: Option<ObjectPointer<'a>>,
    pub(crate) wrapped: Option<JsPrimitive>,
    pub(crate) callable: Option<FunctionReference<'a>>,
    pub(crate) construct: Option<FunctionReference<'a>>,
}

impl<'a> Default for Property<'a> {
    fn default() -> Self {
        Property::DataDescriptor {
            value: ObjectValue::Undefined,
            writable: true,
            configurable: true,
            enumerable: false,
        }
    }
}

impl Default for PropertyHasher {
    fn default() -> Self {
        PropertyHasher { i: 0 }
    }
}

#[derive(Clone, Debug)]
pub enum ObjectValue<'a> {
    Undefined,
    Null,
    Boolean(bool),
    Float(f64),
    String(StringPointer),
    Object(ObjectPointer<'a>),
}

impl<'a> From<RuntimeValue<'a>> for ObjectValue<'a> {
    fn from(value: RuntimeValue<'a>) -> Self {
        match value {
            RuntimeValue::Undefined => ObjectValue::Undefined,
            RuntimeValue::Null => ObjectValue::Null,
            RuntimeValue::Boolean(v) => ObjectValue::Boolean(v),
            RuntimeValue::Float(f) => ObjectValue::Float(f),
            RuntimeValue::String(str) => ObjectValue::String(str),
            RuntimeValue::Object(obj) => ObjectValue::Object(obj),
            RuntimeValue::Reference(_) => panic!("References cannot be stored in an object"),
            RuntimeValue::Internal(_) => panic!("Internal values cannot be stored in an object"),
        }
    }
}

impl<'a> From<ObjectValue<'a>> for RuntimeValue<'a> {
    fn from(value: ObjectValue<'a>) -> Self {
        match value {
            ObjectValue::Undefined => RuntimeValue::Undefined,
            ObjectValue::Null => RuntimeValue::Null,
            ObjectValue::Boolean(v) => RuntimeValue::Boolean(v),
            ObjectValue::Float(f) => RuntimeValue::Float(f),
            ObjectValue::String(str) => RuntimeValue::String(str),
            ObjectValue::Object(obj) => RuntimeValue::Object(obj),
        }
    }
}

impl<'a> Debug for JsObject<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("[Object object]");

        for (key, value) in &self.properties {
            match value {
                Property::DataDescriptor { value, .. } => {
                    debug.field(&format!("[{}]", key), value);
                }
                Property::AccessorDescriptor { .. } => {
                    // debug.field(&format!("[{}]", key), "complex");
                }
            }
        }

        if let Some(prototype) = &self.prototype {
            debug.field("[[Prototype]]", prototype);
        }

        if self.callable.is_some() {
            debug.field("[[Callable]]", &"function() {}".to_owned());
        }

        debug.finish()
    }
}

#[derive(Clone)]
pub(crate) struct PropertyHasher {
    pub(crate) i: u64,
}

impl Hasher for PropertyHasher {
    fn finish(&self) -> u64 {
        self.i
    }

    fn write(&mut self, _bytes: &[u8]) {
        unimplemented!()
    }

    fn write_u32(&mut self, i: u32) {
        self.i = u64::from(i)
    }

    fn write_u64(&mut self, i: u64) {
        self.i = i
    }
}

impl BuildHasher for PropertyHasher {
    type Hasher = PropertyHasher;

    fn build_hasher(&self) -> Self::Hasher {
        PropertyHasher { i: 0 }
    }
}

pub trait FunctionObject<'a> {
    fn name(&self) -> Option<&JsPrimitiveString>;
    fn construct(&self) -> Option<&FunctionReference<'a>>;
    fn callable(&self) -> Option<&FunctionReference<'a>>;
    fn prototype(&self) -> Option<ObjectPointer<'a>>;
    fn is_class_constructor(&self) -> bool;
}

pub(crate) struct JsObjectBuilder<'a, 'b> {
    inner: &'b mut JsObject<'a>,
    pointer: ObjectPointer<'a>,
}

impl<'a, 'b> JsObjectBuilder<'a, 'b> {
    pub fn with_callable(&mut self, function: impl Into<FunctionReference<'a>>) -> &mut Self {
        self.inner.callable = Some(function.into());
        self
    }

    pub fn with_construct(&mut self, function: impl Into<FunctionReference<'a>>) -> &mut Self {
        self.inner.construct = Some(function.into());
        self
    }

    pub fn with_prototype(&mut self, prototype: ObjectPointer<'a>) -> &mut Self {
        self.inner.prototype = Some(prototype);
        self
    }

    pub fn with_name(&mut self, name: impl Into<JsPrimitiveString>) -> &mut Self {
        self.inner.name = Some(name.into());
        self
    }

    pub fn with_wrapped_value(&mut self, value: impl Into<JsPrimitive>) -> &mut Self {
        self.inner.wrapped = Some(value.into());
        self
    }

    pub fn with_indexed_properties(&mut self, properties: Vec<RuntimeValue<'a>>) -> &mut Self {
        self.inner.indexed_properties = properties;
        self
    }

    pub fn with_property(
        &mut self,
        key: JsPrimitiveString,
        value: impl Into<RuntimeValue<'a>>,
    ) -> &mut Self {
        self.inner.set(key, value.into());
        self
    }

    pub(crate) fn build(&mut self) -> ObjectPointer<'a> {
        self.pointer
    }
}

impl<'a> Default for JsObject<'a> {
    fn default() -> Self {
        JsObject {
            properties: HashMap::with_hasher(PropertyHasher { i: 0 }),
            indexed_properties: Vec::new(),
            name: None,
            prototype: None,
            wrapped: None,
            callable: None,
            construct: None,
        }
    }
}

impl<'a> JsObject<'a> {
    pub(crate) fn builder<'b>(pool: &'b mut ObjectPool<'a>) -> JsObjectBuilder<'a, 'b> {
        let object = pool.allocate(JsObject::new());
        let inner = pool.get_mut(object);

        JsObjectBuilder {
            inner,
            pointer: object,
        }
    }

    pub(crate) fn get_indexed_property(&self, key: usize) -> RuntimeValue<'a> {
        self.indexed_properties
            .get(key)
            .cloned()
            .unwrap_or_default()
    }

    #[must_use]
    pub fn new() -> Self {
        JsObject::default()
    }

    #[must_use]
    pub fn is_class_constructor(&self) -> bool {
        self.construct.is_some()
    }

    pub fn wrap(&mut self, value: impl Into<JsPrimitive>) {
        self.wrapped = Some(value.into());
    }

    #[must_use]
    pub fn get_wrapped_value(&self) -> Option<RuntimeValue<'a>> {
        let value = self;
        value.wrapped.as_ref().cloned().map(|v| v.into())
    }

    pub fn set_callable(&mut self, value: impl Into<FunctionReference<'a>>) {
        self.callable = Some(value.into());
    }

    pub fn set_construct(&mut self, value: impl Into<FunctionReference<'a>>) {
        self.construct = Some(value.into());
    }

    pub fn set_wrapped_value(&mut self, value: impl Into<JsPrimitive>) {
        self.wrapped = Some(value.into());
    }

    pub fn set_prototype(&mut self, prototype: ObjectPointer<'a>) {
        self.prototype = Some(prototype);
    }

    pub fn set_name(&mut self, name: impl Into<JsPrimitiveString>) {
        self.name = Some(name.into());
    }

    pub fn set_indexed_properties(&mut self, properties: Vec<RuntimeValue<'a>>) {
        self.indexed_properties = properties;
    }

    #[must_use]
    pub fn get_indexed_properties(&self) -> &Vec<RuntimeValue<'a>> {
        &self.indexed_properties
    }

    pub fn get_mut_indexed_properties(&mut self) -> &mut Vec<RuntimeValue<'a>> {
        &mut self.indexed_properties
    }

    #[must_use]
    pub fn prototype(&self) -> Option<ObjectPointer<'a>> {
        self.prototype
    }

    pub fn set(&mut self, key: JsPrimitiveString, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

        if matches!(value, RuntimeValue::Internal(_)) {
            panic!("Can't write internal values to an object")
        }

        self.properties.insert(key, Property::value(value));
    }

    pub(crate) fn define_property(
        &mut self,
        key: JsPrimitiveString,
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
        enumerable: bool,
        configurable: bool,
    ) {
        self.properties.insert(
            key.into(),
            Property::AccessorDescriptor {
                getter,
                setter,
                enumerable,
                configurable,
            },
        );
    }

    pub(crate) fn define_value(
        &mut self,
        key: impl Into<JsPrimitiveString>,
        value: impl Into<RuntimeValue<'a>>,
    ) {
        self.properties
            .insert(key.into(), Property::value(value.into()));
    }

    pub(crate) fn define_value_property(
        &mut self,
        key: JsPrimitiveString,
        value: impl Into<RuntimeValue<'a>>,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    ) {
        self.properties.insert(
            key,
            Property::DataDescriptor {
                value: value.into().into(),
                writable,
                enumerable,
                configurable,
            },
        );
    }
}

impl<'a> From<ObjectPointer<'a>> for RuntimeValue<'a> {
    fn from(obj: ObjectPointer<'a>) -> Self {
        RuntimeValue::Object(obj)
    }
}

impl<'a> PartialEq for JsObject<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

#[derive(Clone)]
pub(crate) enum Property<'a> {
    DataDescriptor {
        value: ObjectValue<'a>,
        configurable: bool,
        enumerable: bool,
        writable: bool,
    },
    AccessorDescriptor {
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
        enumerable: bool,
        configurable: bool,
    },
}

impl<'a> Property<'a> {
    pub fn value(value: RuntimeValue<'a>) -> Property<'a> {
        Property::DataDescriptor {
            value: value.into(),
            configurable: true,
            enumerable: true,
            writable: true,
        }
    }
}

impl<'a> Debug for Property<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Property::DataDescriptor { value, .. } => value.fmt(f),
            Property::AccessorDescriptor { .. } => {
                f.write_fmt(format_args!("{}", "property".blue()))
            }
        }
    }
}

impl<'a> DebugRepresentation<'a> for JsObject<'a> {
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            renderer.formatter.write_fmt(format_args!("{} ", name))?;
        }

        if renderer.representation != Representation::Compact {
            renderer.formatter.write_char('{')?;
            for (k, v) in self.indexed_properties.iter().enumerate() {
                renderer.formatter.write_str(&k.to_string())?;
                renderer.formatter.write_str(": ")?;
                renderer.render(v)?;
                renderer.formatter.write_str(", ")?;
            }

            for (k, v) in self.properties.iter() {
                renderer.formatter.write_str(&format!("{}", k))?;
                renderer.formatter.write_str(": ")?;
                match v {
                    Property::DataDescriptor { value, .. } => {
                        RuntimeValue::render(&value.clone().into(), renderer)?
                    }
                    Property::AccessorDescriptor { .. } => renderer.literal("complex")?,
                };
                renderer.formatter.write_str(", ")?;
            }

            renderer.formatter.write_char('}')?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::values::object::{Property, PropertyHasher};
    use crate::{JsPrimitiveString, RuntimeValue};
    use std::collections::HashMap;

    #[test]
    fn test() {
        println!(
            "{}",
            std::mem::size_of::<HashMap<JsPrimitiveString, Property, PropertyHasher>>()
        );

        println!("{}", std::mem::size_of::<RuntimeValue>());
    }
}
