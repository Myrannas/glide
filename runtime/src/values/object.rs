use better_any::Tid;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Formatter, Write};
use std::hash::{BuildHasher, Hasher};
use std::rc::Rc;

use crate::debugging::{DebugRepresentation, Renderer, Representation, X};

use super::string::JsPrimitiveString;
use crate::object_pool::{ObjectPointer, ObjectPool};

use crate::values::function::FunctionReference;
use crate::values::nan::Value;

// pub(crate) struct  NativeHandle<'a> {
//     fn downcast<'b, T: 'a>(&'b self) -> Option<&'b T>;
//     fn downcast_mut<'b, T: 'a>(&'b self) -> Option<&'b mut T>;
// }

#[derive(Clone)]
pub struct JsObject<'a> {
    pub(crate) properties: HashMap<JsPrimitiveString, Property<'a>, PropertyHasher>,
    pub(crate) private_properties: Vec<Value<'a>>,
    pub(crate) indexed_properties: Vec<Value<'a>>,
    pub(crate) name: Option<JsPrimitiveString>,
    pub(crate) prototype: Option<ObjectPointer<'a>>,
    pub(crate) wrapped: Option<Value<'a>>,
    pub(crate) callable: Option<FunctionReference<'a>>,
    pub(crate) construct: Option<FunctionReference<'a>>,
    pub(crate) native_handle: Option<Rc<RefCell<dyn Tid<'a>>>>,
}

impl<'a> Default for Property<'a> {
    fn default() -> Self {
        Property::DataDescriptor {
            value: Value::UNDEFINED,
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

    pub fn with_wrapped_value(&mut self, value: Value<'a>) -> &mut Self {
        self.inner.wrapped = Some(value);
        self
    }

    pub fn with_indexed_properties(&mut self, properties: Vec<Value<'a>>) -> &mut Self {
        self.inner.indexed_properties = properties;
        self
    }

    pub fn with_property(
        &mut self,
        key: JsPrimitiveString,
        value: impl Into<Value<'a>>,
    ) -> &mut Self {
        self.inner.set(key, value.into());
        self
    }

    pub fn with_private_properties(&mut self, size: usize) -> &mut Self {
        self.inner.private_properties = Vec::with_capacity(size);
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
            native_handle: None,
            private_properties: Vec::new(),
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

    pub(crate) fn get_indexed_property(&self, key: usize) -> Value<'a> {
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

    pub fn wrap(&mut self, value: Value<'a>) {
        self.wrapped = Some(value);
    }

    #[must_use]
    pub fn get_wrapped_value(&self) -> Option<Value<'a>> {
        self.wrapped
    }

    pub fn set_callable(&mut self, value: impl Into<FunctionReference<'a>>) {
        self.callable = Some(value.into());
    }

    pub fn set_construct(&mut self, value: impl Into<FunctionReference<'a>>) {
        self.construct = Some(value.into());
    }

    pub fn set_wrapped_value(&mut self, value: Value<'a>) {
        self.wrapped = Some(value);
    }

    pub fn set_prototype(&mut self, prototype: ObjectPointer<'a>) {
        self.prototype = Some(prototype);
    }

    pub fn set_name(&mut self, name: impl Into<JsPrimitiveString>) {
        self.name = Some(name.into());
    }

    pub fn set_indexed_properties(&mut self, properties: Vec<Value<'a>>) {
        self.indexed_properties = properties;
    }

    #[must_use]
    pub fn get_indexed_properties(&self) -> &Vec<Value<'a>> {
        &self.indexed_properties
    }

    pub fn get_mut_indexed_properties(&mut self) -> &mut Vec<Value<'a>> {
        &mut self.indexed_properties
    }

    #[must_use]
    pub fn prototype(&self) -> Option<ObjectPointer<'a>> {
        self.prototype
    }

    pub fn set(&mut self, key: JsPrimitiveString, value: impl Into<Value<'a>>) {
        let value = value.into();

        if value.is_local() {
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
            key,
            Property::AccessorDescriptor {
                getter,
                setter,
                enumerable,
                configurable,
            },
        );
    }

    pub(crate) fn define_value_property(
        &mut self,
        key: JsPrimitiveString,
        value: Value<'a>,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    ) {
        self.properties.insert(
            key,
            Property::DataDescriptor {
                value,
                writable,
                enumerable,
                configurable,
            },
        );
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
        value: Value<'a>,
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
    pub fn value(value: Value<'a>) -> Property<'a> {
        Property::DataDescriptor {
            value,
            configurable: true,
            enumerable: true,
            writable: true,
        }
    }

    pub fn is_enumerable(&self) -> bool {
        match self {
            Property::DataDescriptor { enumerable, .. } => *enumerable,
            Property::AccessorDescriptor { enumerable, .. } => *enumerable,
        }
    }
}

impl<'a> DebugRepresentation<'a> for JsObject<'a> {
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            renderer
                .formatter
                .write_fmt(format_args!("{} ", renderer.realm.strings.get(*name)))?;
        }

        if self.callable.is_some() || self.construct.is_some() {
            let name = self
                .name
                .map_or("", |s| renderer.realm.strings.get(s).as_ref());

            renderer.formatter.write_str("[Function: ")?;
            renderer.formatter.write_str(name)?;

            renderer.formatter.write_char(']')?;
        }

        if renderer.representation != Representation::Compact {
            renderer.formatter.write_char('{')?;

            let mut first_property = true;
            for (k, v) in self.indexed_properties.iter().enumerate() {
                if first_property {
                    first_property = false;
                } else {
                    renderer.formatter.write_str(", ")?;
                }
                renderer.formatter.write_str(&k.to_string())?;
                renderer.formatter.write_str(": ")?;
                renderer.render(v)?;
            }

            first_property = true;
            for (k, v) in &self.properties {
                if first_property {
                    first_property = false;
                } else {
                    renderer.formatter.write_str(", ")?;
                }

                let key = renderer.realm.strings.get(*k);
                renderer.formatter.write_str(key.as_ref())?;
                renderer.formatter.write_str(": ")?;
                match v {
                    Property::DataDescriptor { value, .. } => Value::render(value, renderer)?,
                    Property::AccessorDescriptor { .. } => renderer.literal("complex")?,
                };
            }

            renderer.formatter.write_char('}')?;

            // if let Some(prototype) = self.prototype {
            //     renderer.formatter.write_str("__proto__: [")?;
            //     renderer.render(&prototype)?;
            //     renderer.formatter.write_str("], ")?;
            // }
        }

        if self.indexed_properties.len() > 0 {
            renderer.render(&self.indexed_properties)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::values::object::{Property, PropertyHasher};
    use crate::{JsPrimitiveString, Value};
    use std::collections::HashMap;

    #[test]
    fn test() {
        println!(
            "{}",
            std::mem::size_of::<HashMap<JsPrimitiveString, Property, PropertyHasher>>()
        );

        println!("{}", std::mem::size_of::<Value>());
    }
}
