use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Pointer, Write};
use std::hash::{BuildHasher, Hasher};
use std::rc::Rc;

use colored::Colorize;

use crate::debugging::{DebugRepresentation, Renderer, Representation};
use crate::result::JsResult;
use crate::values::primitives::JsPrimitive;
use crate::vm::JsThread;

use super::string::JsPrimitiveString;
use super::value::RuntimeValue;
use crate::object_pool::{ObjectPointer, ObjectPool};
use crate::values::function::FunctionReference;

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

impl<'a> Debug for JsObject<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = if let Some(name) = &self.name {
            name.clone()
        } else {
            "[object Object]".into()
        };

        let mut debug = f.debug_struct(name.as_ref());

        for (key, value) in self.properties.iter() {
            debug.field(key.as_ref(), value);
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

pub trait FunctionObject<'a> {
    fn name(&self) -> Option<&JsPrimitiveString>;
    fn construct(&self) -> Option<&FunctionReference<'a>>;
    fn callable(&self) -> Option<&FunctionReference<'a>>;
    fn prototype(&self) -> Option<ObjectPointer<'a>>;
    fn is_class_constructor(&self) -> bool;
}

struct ConstructorFunctionObject<'a> {
    object: JsObject<'a>,
}

impl<'a> FunctionObject<'a> for ConstructorFunctionObject<'a> {
    fn name(&self) -> Option<&JsPrimitiveString> {
        self.object.name.as_ref()
    }

    fn construct<'b>(&'b self) -> Option<&FunctionReference<'a>> {
        self.object.construct.as_ref()
    }

    /*

    https://262.ecma-international.org/11.0/#sec-ecmascript-function-objects-call-thisargument-argumentslist

        Assert: F is an ECMAScript function object.
        If F.[[IsClassConstructor]] is true, throw a TypeError exception.
        Let callerContext be the running execution context.
        Let calleeContext be PrepareForOrdinaryCall(F, undefined).
        Assert: calleeContext is now the running execution context.
        Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
        Let result be OrdinaryCallEvaluateBody(F, argumentsList).
        Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
        If result.[[Type]] is return, return NormalCompletion(result.[[Value]]).
        ReturnIfAbrupt(result).
        Return NormalCompletion(undefined).
    */
    fn callable<'b>(&'b self) -> Option<&FunctionReference<'a>> {
        self.object.callable.as_ref()
    }

    fn prototype<'b>(&'b self) -> Option<ObjectPointer<'a>> {
        self.object.prototype
    }

    fn is_class_constructor(&self) -> bool {
        self.construct().is_some() && self.callable().is_none()
    }
}

pub(crate) struct JsObjectBuilder<'a> {
    inner: JsObject<'a>,
}

impl<'a> JsObjectBuilder<'a> {
    pub fn with_callable(mut self, function: impl Into<FunctionReference<'a>>) -> Self {
        self.inner.callable = Some(function.into());
        self
    }

    pub fn with_construct(mut self, function: impl Into<FunctionReference<'a>>) -> Self {
        self.inner.construct = Some(function.into());
        self
    }

    pub fn with_prototype(mut self, prototype: ObjectPointer<'a>) -> Self {
        self.inner.prototype = Some(prototype);
        self
    }

    pub fn with_name(mut self, name: impl Into<JsPrimitiveString>) -> Self {
        self.inner.name = Some(name.into());
        self
    }

    pub fn with_wrapped_value(mut self, value: impl Into<JsPrimitive>) -> Self {
        self.inner.wrapped = Some(value.into());
        self
    }

    pub fn with_indexed_properties(mut self, properties: Vec<RuntimeValue<'a>>) -> Self {
        self.inner.indexed_properties = properties;
        self
    }

    pub fn with_property(
        mut self,
        key: impl Into<JsPrimitiveString>,
        value: impl Into<RuntimeValue<'a>>,
    ) -> Self {
        self.inner
            .properties
            .insert(key.into(), Property::value(value.into()));
        self
    }

    pub(crate) fn build(self, object_pool: &mut impl ObjectPool<'a>) -> ObjectPointer<'a> {
        object_pool.allocate(self.inner)
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
    pub(crate) fn builder() -> JsObjectBuilder<'a> {
        JsObjectBuilder {
            inner: Default::default(),
        }
    }

    pub(crate) fn get_property(
        &self,
        pool: &impl ObjectPool<'a>,
        key: &JsPrimitiveString,
    ) -> Option<Property<'a>> {
        self.properties.get(key).cloned().or_else(|| {
            if let Some(prototype) = &self.prototype {
                prototype.get_property(pool, key)
            } else {
                None
            }
        })
    }

    pub fn new() -> Self {
        Default::default()
    }

    pub fn function(&self) -> Option<impl FunctionObject<'a>> {
        let is_callable = self.is_function();

        if is_callable {
            Some(ConstructorFunctionObject {
                object: self.clone(),
            })
        } else {
            None
        }
    }

    pub(crate) fn is_function(&self) -> bool {
        let inner = self;

        inner.callable.is_some() || inner.construct.is_some()
    }

    pub fn is_class_constructor(&self) -> bool {
        self.construct.is_some()
    }

    pub fn wrap(&mut self, value: impl Into<JsPrimitive>) {
        self.wrapped = Some(value.into());
    }

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

    pub fn get_indexed_properties(&self) -> &Vec<RuntimeValue<'a>> {
        &self.indexed_properties
    }

    pub fn get_mut_indexed_properties(&mut self) -> &mut Vec<RuntimeValue<'a>> {
        &mut self.indexed_properties
    }

    pub fn prototype(&self) -> Option<ObjectPointer<'a>> {
        self.prototype
    }

    pub fn has<'b>(&self, key: &JsPrimitiveString) -> bool {
        self.properties.contains_key(key)
    }

    pub fn set(&mut self, key: &JsPrimitiveString, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

        if matches!(value, RuntimeValue::Internal(_)) {
            panic!("Can't write internal values to an object")
        }

        self.properties.insert(key.clone(), Property::value(value));
    }

    pub fn set_indexed(&mut self, key: usize, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

        if key < 10000 {
            let indexed_properties = &mut self.indexed_properties;

            if indexed_properties.capacity() > 0 {
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

        self.set(&key.to_string().into(), value)
    }

    pub(crate) fn define_property(
        &mut self,
        key: impl Into<JsPrimitiveString>,
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
        key: impl Into<JsPrimitiveString>,
        value: impl Into<RuntimeValue<'a>>,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    ) {
        self.properties.insert(
            key.into(),
            Property::DataDescriptor {
                value: value.into(),
                writable,
                enumerable,
                configurable,
            },
        );
    }

    pub(crate) fn read_simple_property(
        &self,
        key: impl Into<JsPrimitiveString>,
    ) -> RuntimeValue<'a> {
        let key = key.into();

        match self.properties.get(&key) {
            Some(Property::DataDescriptor { value, .. }) => value.clone(),
            _ => unreachable!(),
        }
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
        value: RuntimeValue<'a>,
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

pub(crate) struct PropertyForTarget<'a> {
    property: Property<'a>,
    pub(crate) object: ObjectPointer<'a>,
}

impl<'a> PropertyForTarget<'a> {
    pub(crate) fn get(self, thread: &mut JsThread<'a>) -> JsResult<'a> {
        match self.property {
            Property::DataDescriptor { value, .. } => Ok(value),
            Property::AccessorDescriptor {
                getter: Some(fn_reference),
                ..
            } => thread.call_from_native(self.object, fn_reference, 0, false),
            _ => panic!("not readable"),
        }
    }

    fn set(self, thread: &mut JsThread<'a>, target: ObjectPointer<'a>) -> JsResult<'a> {
        todo!("")
        // match self.property {
        //     Property::DataDescriptor { value, .. } => {
        //         // thread.realm.objects.get_mut(self.object).inner.borrow_mut().properties.
        //     },
        //     Property::AccessorDescriptor {
        //         getter: Some(fn_reference),
        //         ..
        //     } => thread.call_from_native(target, fn_reference, 0, false),
        //     _ => panic!("not readable"),
        // }
    }
}

impl<'a> Property<'a> {
    fn value(value: RuntimeValue<'a>) -> Property<'a> {
        Property::DataDescriptor {
            value,
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

impl<'a> DebugRepresentation for JsObject<'a> {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
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
                renderer.formatter.write_str(k.as_ref())?;
                renderer.formatter.write_str(": ")?;
                match v {
                    Property::DataDescriptor { value, .. } => renderer.render(value)?,
                    Property::AccessorDescriptor { .. } => renderer.literal("complex")?,
                };
                renderer.formatter.write_str(", ")?;
            }

            renderer.formatter.write_char('}')?;
        }

        Ok(())
    }
}
