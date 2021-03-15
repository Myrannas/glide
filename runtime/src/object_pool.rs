use crate::debugging::{DebugRepresentation, Renderer};
use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::values::object::Property;
use crate::values::primitives::JsPrimitive;
use crate::{FunctionObject, JsObject, JsPrimitiveString, JsThread, RuntimeValue};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

type ObjectId = u32;

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct ObjectPointer<'a> {
    index: ObjectId,
    phantom_data: std::marker::PhantomData<JsObjectPool<'a>>,
}

impl<'a> DebugRepresentation for ObjectPointer<'a> {
    fn render(&self, renderer: &mut Renderer) -> std::fmt::Result {
        renderer
            .formatter
            .write_fmt(format_args!("Object@{}", self.index))
    }
}

impl<'a> Display for ObjectPointer<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Object@{}", self.index))
    }
}

impl<'a> ObjectPointer<'a> {
    pub(crate) fn get_object<'b>(&'b self, pool: &'b impl ObjectPool<'a>) -> &'b JsObject<'a> {
        pool.get(self)
    }

    pub(crate) fn get_mut_object<'b>(
        &'b self,
        pool: &'b mut impl ObjectPool<'a>,
    ) -> &'b mut JsObject<'a> {
        pool.get_mut(self)
    }

    pub(crate) fn set_prototype(
        &self,
        pool: &mut impl ObjectPool<'a>,
        prototype: ObjectPointer<'a>,
    ) {
        pool.get_mut(self).set_prototype(prototype)
    }

    pub fn set_construct(
        &self,
        pool: &mut impl ObjectPool<'a>,
        construct: impl Into<FunctionReference<'a>>,
    ) {
        pool.get_mut(self).set_construct(construct)
    }

    pub fn set_callable(
        &self,
        pool: &mut impl ObjectPool<'a>,
        construct: impl Into<FunctionReference<'a>>,
    ) {
        pool.get_mut(self).set_callable(construct)
    }

    pub fn get_value(
        &self,
        thread: &mut JsThread<'a>,
        key: &JsPrimitiveString,
    ) -> JsResult<'a, RuntimeValue<'a>> {
        if key.as_ref() == "prototype" {
            return Ok(self
                .get_prototype(thread)
                .map(RuntimeValue::Object)
                .unwrap_or_default());
        }

        let object = thread.realm.objects.get(self);

        let property = object.get_property(&thread.realm.objects, &key);

        let result = match property {
            Some(Property::DataDescriptor { value, .. }) => value,
            Some(Property::AccessorDescriptor {
                getter: Some(function),
                ..
            }) => thread.call_from_native(*self, function, 0, false)?,
            _ => RuntimeValue::Undefined,
        };

        Ok(result)
    }

    pub fn get_indexed(&self, thread: &mut JsThread<'a>, key: usize) -> JsResult<'a> {
        let object = thread.realm.objects.get(self);

        if let Some(JsPrimitive::String(str)) = &object.wrapped {
            return Ok(str
                .inner
                .get(key..key)
                .map(|v| v.to_string())
                .unwrap_or_else(|| " ".to_string())
                .into());
        }

        let result = if object.indexed_properties.capacity() > 0 {
            object.get_indexed_property(key)
        } else {
            let property = object.get_property(&thread.realm.objects, &key.to_string().into());

            match property {
                Some(Property::DataDescriptor { value, .. }) => value,
                Some(Property::AccessorDescriptor {
                    getter: Some(function),
                    ..
                }) => thread.call_from_native(*self, function, 0, false)?,
                _ => RuntimeValue::Undefined,
            }
        };

        Ok(result)
    }

    pub(crate) fn get_property(
        &self,
        pool: &impl ObjectPool<'a>,
        key: &JsPrimitiveString,
    ) -> Option<Property<'a>> {
        let object = pool.get(self);

        object.get_property(pool, key)
    }

    pub fn has(&self, objects: &impl ObjectPool<'a>, key: &JsPrimitiveString) -> bool {
        let object = objects.get(self);

        object.has(key)
    }

    pub fn set(&self, thread: &mut JsThread<'a>, key: &JsPrimitiveString, value: RuntimeValue<'a>) {
        let object = thread.realm.objects.get_mut(self);

        object.set(key, value)
    }

    pub fn set_indexed(&self, thread: &mut JsThread<'a>, key: usize, value: RuntimeValue<'a>) {
        let object = thread.realm.objects.get_mut(self);

        object.set_indexed(key, value)
    }

    pub fn define_value_property(
        &self,
        pool: &mut impl ObjectPool<'a>,
        key: impl Into<JsPrimitiveString>,
        value: impl Into<RuntimeValue<'a>>,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    ) {
        let object = pool.get_mut(self);

        object.define_value_property(key, value, writable, enumerable, configurable)
    }

    pub fn unwrap(&self, thread: &mut JsThread<'a>) -> Option<RuntimeValue<'a>> {
        let object = thread.realm.objects.get(self);

        object.get_wrapped_value()
    }

    pub fn wrap(&self, thread: &mut JsThread<'a>, value: impl Into<RuntimeValue<'a>>) {
        let object = thread.realm.objects.get_mut(self);

        object.set_wrapped_value(value.into());
    }

    pub fn call(&self, thread: &mut JsThread<'a>, args: &[RuntimeValue<'a>]) -> JsResult<'a> {
        let function_reference = self
            .get_callable(thread)
            .cloned()
            .ok_or_else(|| {
                thread.realm.errors.new_type_error(
                    &mut thread.realm.objects,
                    format!("{} is not a function", self),
                )
            })?
            .clone();

        for arg in args {
            thread.push_stack(arg.clone());
        }

        thread.call_from_native(self.clone(), function_reference, args.len(), false)
    }

    pub fn get_prototype(&self, thread: &mut JsThread<'a>) -> Option<ObjectPointer<'a>> {
        thread.get(self).prototype()
    }

    pub fn get_name<'b>(&'b self, pool: &'b impl ObjectPool<'a>) -> Option<&'b JsPrimitiveString> {
        let object = pool.get(self);

        object.name.as_ref()
    }

    pub fn get_construct<'b>(
        &'b self,
        pool: &'b impl ObjectPool<'a>,
    ) -> Option<&FunctionReference<'a>> {
        let object = pool.get(self);

        object.construct.as_ref()
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
    pub fn get_callable<'b>(
        &'b self,
        pool: &'b impl ObjectPool<'a>,
    ) -> Option<&FunctionReference<'a>> {
        let object = pool.get(self);

        object.callable.as_ref()
    }

    pub fn is_class_constructor(&self, pool: &impl ObjectPool<'a>) -> bool {
        let object = pool.get(self);

        object.construct.is_some() && object.callable.is_none()
    }

    pub fn is_callable(&self, pool: &impl ObjectPool<'a>) -> bool {
        let object = pool.get(self);

        object.construct.is_some() || object.callable.is_some()
    }
}

#[derive(Clone)]
pub struct JsObjectPool<'a> {
    objects: Vec<JsObject<'a>>,
}

pub trait ObjectPool<'a> {
    fn get(&self, index: &ObjectPointer<'a>) -> &JsObject<'a>;
    fn get_mut(&mut self, index: &ObjectPointer<'a>) -> &mut JsObject<'a>;
    fn allocate(&mut self, object: JsObject<'a>) -> ObjectPointer<'a>;
}

impl<'a> JsObjectPool<'a> {
    pub(crate) fn new() -> JsObjectPool<'a> {
        JsObjectPool {
            objects: Vec::with_capacity(256),
        }
    }
}

impl<'a> ObjectPool<'a> for JsObjectPool<'a> {
    #[inline(always)]
    fn get(&self, index: &ObjectPointer<'a>) -> &JsObject<'a> {
        &self.objects[index.index as usize]
    }

    #[inline(always)]
    fn get_mut(&mut self, index: &ObjectPointer<'a>) -> &mut JsObject<'a> {
        &mut self.objects[index.index as usize]
    }

    #[inline(always)]
    fn allocate(&mut self, object: JsObject<'a>) -> ObjectPointer<'a> {
        let index = self.objects.len();
        self.objects.push(object);

        ObjectPointer {
            index: index as u32,
            phantom_data: PhantomData,
        }
    }
}
