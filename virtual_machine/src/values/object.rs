use std::cell::{RefCell, RefMut};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Pointer, Write};
use std::hash::{BuildHasher, Hasher};
use std::rc::Rc;

use colored::Colorize;

use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::function::FunctionReference;
use crate::result::JsResult;
use crate::values::primitives::JsPrimitive;
use crate::vm::JsThread;

use super::string::JsPrimitiveString;
use super::value::RuntimeValue;

#[derive(Clone)]
pub struct JsObject<'a> {
    inner: Rc<RefCell<JsObjectInner<'a>>>,
}

impl<'a> Debug for JsObject<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let inner = &self.inner.borrow();

        let name = if let Some(name) = &inner.name {
            name.as_str()
        } else {
            "[object Object]"
        };

        let mut debug = f.debug_struct(name);

        for (key, value) in inner.properties.iter() {
            debug.field(key.as_ref(), value);
        }

        if let Some(prototype) = &inner.prototype {
            debug.field("[[Prototype]]", prototype);
        }

        if let Some(_) = &inner.callable {
            debug.field("[[Callable]]", &"function() {}".to_owned());
        }

        debug.finish()
    }
}

#[derive(Debug)]
struct JsObjectInner<'a> {
    pub(crate) properties: HashMap<JsPrimitiveString, Property<'a>, PropertyHasher>,
    pub(crate) indexed_properties: Option<Vec<RuntimeValue<'a>>>,
    pub(crate) name: Option<String>,
    pub(crate) prototype: Option<JsObject<'a>>,
    pub(crate) wrapped: Option<JsPrimitive>,
    pub(crate) callable: Option<FunctionReference<'a>>,
}

struct PropertyHasher {
    i: u64,
}

impl Hasher for PropertyHasher {
    fn finish(&self) -> u64 {
        self.i
    }

    fn write(&mut self, _bytes: &[u8]) {
        unimplemented!()
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

impl<'a> JsObject<'a> {
    fn get_property(&self, key: &JsPrimitiveString) -> Option<Property<'a>> {
        self.inner
            .borrow()
            .properties
            .get(key)
            .cloned()
            .or_else(|| {
                if let Some(prototype) = &self.inner.borrow().prototype {
                    prototype.get_property(key)
                } else {
                    None
                }
            })
    }

    pub fn new() -> Self {
        JsObject {
            inner: Rc::new(RefCell::new(JsObjectInner {
                properties: HashMap::with_hasher(PropertyHasher { i: 0 }),
                indexed_properties: None,
                name: None,
                prototype: None,
                wrapped: None,
                callable: None,
            })),
        }
    }

    pub fn is_callable(&self) -> bool {
        self.inner.borrow().callable.is_some()
    }

    pub fn get_callable(&self) -> Option<FunctionReference<'a>> {
        self.inner.borrow().callable.as_ref().cloned()
    }

    pub fn wrapping(self, value: impl Into<JsPrimitive>) -> Self {
        self.inner.borrow_mut().wrapped = Some(value.into());
        self
    }

    pub fn wrap(&self, value: impl Into<JsPrimitive>) {
        self.inner.borrow_mut().wrapped = Some(value.into());
    }

    pub fn callable(self, value: impl Into<FunctionReference<'a>>) -> Self {
        self.inner.borrow_mut().callable = Some(value.into());
        self
    }

    pub fn get_wrapped_value(&self) -> Option<RuntimeValue<'a>> {
        let value = self.inner.borrow();
        value.wrapped.as_ref().cloned().map(|v| v.into())
    }

    pub fn with_prototype(self, prototype: Self) -> Self {
        self.inner.borrow_mut().prototype = Some(prototype);
        self
    }

    pub fn with_name(self, name: impl Into<String>) -> Self {
        self.inner.borrow_mut().name = Some(name.into());
        self
    }

    pub fn with_indexed_properties(self, properties: Vec<RuntimeValue<'a>>) -> Self {
        self.inner.borrow_mut().indexed_properties = Some(properties);
        self
    }

    pub fn get_indexed_properties(&self) -> RefMut<Option<Vec<RuntimeValue<'a>>>> {
        RefMut::map(self.inner.borrow_mut(), |v| &mut v.indexed_properties)
    }

    pub fn prototype(&self) -> Option<JsObject<'a>> {
        self.inner.borrow().prototype.clone()
    }

    pub fn get<'b>(&self, key: JsPrimitiveString, frame: &'b mut JsThread<'a>) -> JsResult<'a> {
        self.get_borrowed(&key, frame)
    }

    pub fn set(&self, key: JsPrimitiveString, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

        if matches!(value, RuntimeValue::Internal(_)) {
            panic!("Can't write internal values to an object")
        }

        self.inner
            .borrow_mut()
            .properties
            .insert(key, Property::Value(value));
    }

    pub fn set_indexed(&self, key: usize, value: RuntimeValue<'a>) {
        {
            let mut object = self.inner.borrow_mut();

            if let Some(indexed_properties) = &mut object.indexed_properties {
                match key.cmp(&indexed_properties.len()) {
                    Ordering::Less => indexed_properties[key] = value,
                    Ordering::Equal => indexed_properties.push(value),
                    Ordering::Greater => {
                        for _ in indexed_properties.len()..key {
                            indexed_properties.push(RuntimeValue::Undefined)
                        }
                        indexed_properties.push(value)
                    }
                }

                return;
            }
        }

        self.set(key.to_string().into(), value)
    }

    pub(crate) fn define_property(
        &self,
        key: impl Into<JsPrimitiveString>,
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
    ) {
        self.inner
            .borrow_mut()
            .properties
            .insert(key.into(), Property::Complex { getter, setter });
    }

    pub(crate) fn define_value(
        &self,
        key: impl Into<JsPrimitiveString>,
        value: impl Into<RuntimeValue<'a>>,
    ) {
        self.inner
            .borrow_mut()
            .properties
            .insert(key.into(), Property::Value(value.into()));
    }

    pub(crate) fn read_simple_property(
        &self,
        key: impl Into<JsPrimitiveString>,
    ) -> RuntimeValue<'a> {
        let key = key.into();

        match self.inner.borrow().properties.get(&key) {
            Some(Property::Value(value)) => value.clone(),
            _ => unreachable!(),
        }
    }

    fn get_borrowed<'b>(
        &self,
        key: &JsPrimitiveString,
        thread: &'b mut JsThread<'a>,
    ) -> JsResult<'a> {
        let b = self.inner.borrow();

        if "prototype" == key.as_ref() {
            if let Some(prototype) = &b.prototype {
                return Ok(RuntimeValue::Object(prototype.clone()));
            }
        }

        let property = self.get_property(key);

        match property {
            Some(Property::Value(value)) => Ok(value),
            // Some(Property::Complex {
            //     getter: Some(FunctionReference::Custom(CustomFunctionReference { function, .. })),
            //     ..
            // }) => function.execute(
            //     None,
            //     &mut Vec::new(),
            //     0..0,
            //     Some(thread.call_stack.clone()),
            //     &thread.global_this,
            //     target,
            // ),
            Some(Property::Complex {
                getter: Some(FunctionReference::BuiltIn(function)),
                ..
            }) => Ok(function
                .apply_return(0, thread, Some(self.clone()))?
                .unwrap_or(RuntimeValue::Undefined)),
            _ => Ok(RuntimeValue::Undefined),
        }
    }
}

impl<'a> Default for JsObject<'a> {
    fn default() -> Self {
        JsObject::new()
    }
}

impl<'a> From<JsObject<'a>> for RuntimeValue<'a> {
    fn from(obj: JsObject<'a>) -> Self {
        RuntimeValue::Object(obj)
    }
}

impl<'a> PartialEq for JsObject<'a> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Clone)]
pub(crate) enum Property<'a> {
    Value(RuntimeValue<'a>),
    Complex {
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
    },
}

impl<'a> Debug for Property<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Property::Value(v) => v.fmt(f),
            Property::Complex { .. } => f.write_fmt(format_args!("{}", "property".blue())),
        }
    }
}

impl<'a> DebugRepresentation for JsObject<'a> {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
        let value = self.inner.borrow();

        if let Some(name) = &value.name {
            renderer.formatter.write_fmt(format_args!("{} ", name))?;
        }

        if renderer.representation != Representation::Compact {
            renderer.formatter.write_char('{')?;
            if let Some(properties) = &value.indexed_properties {
                for (k, v) in properties.iter().enumerate() {
                    renderer.formatter.write_str(&k.to_string())?;
                    renderer.formatter.write_str(": ")?;
                    renderer.render(v)?;
                    renderer.formatter.write_str(", ")?;
                }
            }

            for (k, v) in value.properties.iter() {
                renderer.formatter.write_str(k.as_ref())?;
                renderer.formatter.write_str(": ")?;
                match v {
                    Property::Value(v) => renderer.render(v)?,
                    Property::Complex { .. } => renderer.literal("complex")?,
                };
                renderer.formatter.write_str(", ")?;
            }

            renderer.formatter.write_char('}')?;
        }

        Ok(())
    }
}
