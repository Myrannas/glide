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
use crate::values::function::FunctionReference;

#[derive(Clone)]
pub struct JsObject<'a> {
    inner: Rc<RefCell<JsObjectInner<'a>>>,
}

impl<'a> Debug for JsObject<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let inner = &self.inner.borrow();

        let name = if let Some(name) = &inner.name {
            name.clone()
        } else {
            "[object Object]".into()
        };

        let mut debug = f.debug_struct(name.as_ref());

        for (key, value) in inner.properties.iter() {
            debug.field(key.as_ref(), value);
        }

        if let Some(prototype) = &inner.prototype {
            debug.field("[[Prototype]]", prototype);
        }

        if inner.callable.is_some() {
            debug.field("[[Callable]]", &"function() {}".to_owned());
        }

        debug.finish()
    }
}

#[derive(Debug)]
struct JsObjectInner<'a> {
    pub(crate) properties: HashMap<JsPrimitiveString, Property<'a>, PropertyHasher>,
    pub(crate) indexed_properties: Option<Vec<RuntimeValue<'a>>>,
    pub(crate) name: Option<JsPrimitiveString>,
    pub(crate) prototype: Option<JsObject<'a>>,
    pub(crate) wrapped: Option<JsPrimitive>,
    pub(crate) callable: Option<FunctionReference<'a>>,
    pub(crate) construct: Option<FunctionReference<'a>>,
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

pub trait FunctionObject<'a> {
    fn name(&self) -> Ref<Option<JsPrimitiveString>>;
    fn construct<'b>(&'b self) -> Ref<'b, Option<FunctionReference<'a>>>;
    fn callable<'b>(&'b self) -> Ref<'b, Option<FunctionReference<'a>>>;
    fn prototype<'b>(&'b self) -> Ref<'b, Option<JsObject<'a>>>;
    fn is_class_constructor(&self) -> bool;
}

struct ConstructorFunctionObject<'a> {
    object: JsObject<'a>,
}

impl<'a> FunctionObject<'a> for ConstructorFunctionObject<'a> {
    fn name(&self) -> Ref<Option<JsPrimitiveString>> {
        Ref::map(self.object.inner.borrow(), |inner| &inner.name)
    }

    fn construct<'b>(&'b self) -> Ref<'b, Option<FunctionReference<'a>>> {
        Ref::map(self.object.inner.borrow(), |inner| &inner.construct)
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
    fn callable<'b>(&'b self) -> Ref<'b, Option<FunctionReference<'a>>> {
        Ref::map(self.object.inner.borrow(), |inner| &inner.callable)
    }

    fn prototype<'b>(&'b self) -> Ref<'b, Option<JsObject<'a>>> {
        Ref::map(self.object.inner.borrow(), |inner| &inner.prototype)
    }

    fn is_class_constructor(&self) -> bool {
        self.construct().is_some() && self.callable().is_none()
    }
}

pub(crate) struct JsObjectBuilder<'a> {
    inner: JsObjectInner<'a>,
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

    pub fn with_prototype(mut self, prototype: JsObject<'a>) -> Self {
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
        self.inner.indexed_properties = Some(properties);
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

    pub(crate) fn build(self) -> JsObject<'a> {
        JsObject {
            inner: Rc::new(RefCell::new(self.inner)),
        }
    }
}

impl<'a> Default for JsObjectInner<'a> {
    fn default() -> Self {
        JsObjectInner {
            properties: HashMap::with_hasher(PropertyHasher { i: 0 }),
            indexed_properties: None,
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

    pub(crate) fn get_property(&self, key: &JsPrimitiveString) -> Option<Property<'a>> {
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
            inner: Rc::new(RefCell::new(Default::default())),
        }
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
        let inner = self.inner.borrow();

        inner.callable.is_some() || inner.construct.is_some()
    }

    pub fn is_class_constructor(&self) -> bool {
        self.inner.borrow().construct.is_some()
    }

    pub fn wrap(&self, value: impl Into<JsPrimitive>) {
        self.inner.borrow_mut().wrapped = Some(value.into());
    }

    pub fn get_wrapped_value(&self) -> Option<RuntimeValue<'a>> {
        let value = self.inner.borrow();
        value.wrapped.as_ref().cloned().map(|v| v.into())
    }

    pub fn set_callable(&self, value: impl Into<FunctionReference<'a>>) {
        self.inner.borrow_mut().callable = Some(value.into());
    }

    pub fn set_construct(&self, value: impl Into<FunctionReference<'a>>) {
        self.inner.borrow_mut().construct = Some(value.into());
    }

    pub fn set_wrapped_value(&self, value: impl Into<JsPrimitive>) {
        self.inner.borrow_mut().wrapped = Some(value.into());
    }

    pub fn set_prototype(&self, prototype: JsObject<'a>) {
        self.inner.borrow_mut().prototype = Some(prototype);
    }

    pub fn set_name(&self, name: impl Into<JsPrimitiveString>) {
        self.inner.borrow_mut().name = Some(name.into());
    }

    pub fn set_indexed_properties(&self, properties: Vec<RuntimeValue<'a>>) {
        self.inner.borrow_mut().indexed_properties = Some(properties);
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

    pub fn has<'b>(&self, key: JsPrimitiveString) -> bool {
        self.inner.borrow().properties.contains_key(&key)
    }

    pub fn get_indexed<'b>(&self, key: usize, frame: &'b mut JsThread<'a>) -> JsResult<'a> {
        let inner = self.inner.borrow();

        if let Some(indexed_properties) = &inner.indexed_properties {
            Ok(indexed_properties.get(key).cloned().unwrap_or_default())
        } else {
            self.get(key.to_string().into(), frame)
        }
    }

    pub fn set(&self, key: JsPrimitiveString, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

        if matches!(value, RuntimeValue::Internal(_)) {
            panic!("Can't write internal values to an object")
        }

        self.inner
            .borrow_mut()
            .properties
            .insert(key, Property::value(value));
    }

    pub fn set_indexed(&self, key: usize, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

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
        enumerable: bool,
        configurable: bool,
    ) {
        self.inner.borrow_mut().properties.insert(
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
        &self,
        key: impl Into<JsPrimitiveString>,
        value: impl Into<RuntimeValue<'a>>,
    ) {
        self.inner
            .borrow_mut()
            .properties
            .insert(key.into(), Property::value(value.into()));
    }

    pub(crate) fn define_value_property(
        &self,
        key: impl Into<JsPrimitiveString>,
        value: impl Into<RuntimeValue<'a>>,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    ) {
        self.inner.borrow_mut().properties.insert(
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

        match self.inner.borrow().properties.get(&key) {
            Some(Property::DataDescriptor { value, .. }) => value.clone(),
            _ => unreachable!(),
        }
    }

    fn get_borrowed<'b>(
        &self,
        key: &JsPrimitiveString,
        thread: &'b mut JsThread<'a>,
    ) -> JsResult<'a> {
        let function = {
            let b = self.inner.borrow();

            if "prototype" == key.as_ref() {
                if let Some(prototype) = &b.prototype {
                    return Ok(RuntimeValue::Object(prototype.clone()));
                }
            }

            let property = self.get_property(key);

            match property {
                Some(Property::DataDescriptor { value, .. }) => return Ok(value),
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
                Some(Property::AccessorDescriptor {
                    getter: Some(FunctionReference::BuiltIn(builtin)),
                    ..
                }) => builtin,
                _ => return Ok(RuntimeValue::Undefined),
            }
        };

        let result = function
            .clone()
            .apply_return(0, thread, Some(self.clone()))?
            .unwrap_or(RuntimeValue::Undefined);

        Ok(result)
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
