use crate::debugging::{DebugRepresentation, Renderer};
use crate::pool::{Pool, PoolPointer};
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::values::nan::{Value, ValueType};
use crate::values::object::Property;
use crate::{JsObject, JsPrimitiveString, JsThread};
use std::cmp::Ordering;

impl<'a> DebugRepresentation<'a> for ObjectPointer<'a> {
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        let obj = renderer.realm.objects.get(*self);

        renderer.literal(&format!("{:?}#", self.inner.to_string()))?;

        JsObject::render(obj, renderer)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct ObjectPointer<'a> {
    inner: PoolPointer<JsObject<'a>>,
}

impl<'a> From<ObjectPointer<'a>> for u32 {
    fn from(value: ObjectPointer<'a>) -> Self {
        value.inner.into()
    }
}

#[derive(Clone)]
pub struct ObjectPool<'a> {
    pool: Pool<JsObject<'a>>,
}

impl<'a> ObjectPool<'a> {
    pub(crate) fn new() -> Self {
        ObjectPool { pool: Pool::new() }
    }

    pub(crate) fn allocate(&mut self, object: JsObject<'a>) -> ObjectPointer<'a> {
        let pointer = self.pool.allocate(object);

        ObjectPointer { inner: pointer }
    }

    pub(crate) fn get(&self, key: ObjectPointer<'a>) -> &JsObject<'a> {
        self.pool.get(key.inner)
    }

    pub(crate) fn get_mut(&mut self, key: ObjectPointer<'a>) -> &mut JsObject<'a> {
        self.pool.get_mut(key.inner)
    }
}

impl<'a> ObjectPointer<'a> {
    pub(crate) fn new(index: u32) -> ObjectPointer<'a> {
        ObjectPointer {
            inner: PoolPointer::new(index),
        }
    }

    pub(crate) fn get_object<'b>(self, pool: &'b ObjectPool<'a>) -> &'b JsObject<'a> {
        pool.get(self)
    }

    pub(crate) fn get_mut_object<'b>(self, pool: &'b mut ObjectPool<'a>) -> &'b mut JsObject<'a> {
        pool.get_mut(self)
    }

    pub(crate) fn set_prototype(self, pool: &mut ObjectPool<'a>, prototype: ObjectPointer<'a>) {
        pool.get_mut(self).set_prototype(prototype)
    }

    pub fn set_construct(
        self,
        pool: &mut ObjectPool<'a>,
        construct: impl Into<FunctionReference<'a>>,
    ) {
        pool.get_mut(self).set_construct(construct)
    }

    pub fn set_callable(
        self,
        pool: &mut ObjectPool<'a>,
        construct: impl Into<FunctionReference<'a>>,
    ) {
        pool.get_mut(self).set_callable(construct)
    }

    pub fn get_value(
        self,
        thread: &mut JsThread<'a>,
        key: JsPrimitiveString,
    ) -> JsResult<'a, Value<'a>> {
        let property = self.get_property(&thread.realm.objects, key);

        let result = match property {
            Some(Property::DataDescriptor { value, .. }) => *value,
            Some(Property::AccessorDescriptor {
                getter: Some(function),
                ..
            }) => {
                let reference = function.clone();
                thread.call_from_native(self.into(), reference, 0, false)?
            }
            _ => Value::UNDEFINED,
        };

        Ok(result)
    }

    pub fn get_indexed(self, thread: &mut JsThread<'a>, key: usize) -> JsResult<'a, Value<'a>> {
        let object = thread.realm.objects.get(self);

        if let Some(ValueType::String(str)) = &object.wrapped.map(Value::get_type) {
            let result = thread
                .realm
                .strings
                .get(*str)
                .as_ref()
                .get(key..key)
                .map(ToOwned::to_owned)
                .and_then(|v| match v {
                    value if value.is_empty() => None,
                    other => Some(Value::from(thread.realm.strings.intern(other))),
                })
                .unwrap_or_default();

            return Ok(result);
        }

        let result = if object.indexed_properties.capacity() > 0 {
            object.get_indexed_property(key)
        } else {
            let pointer = thread.realm.strings.intern(key.to_string());

            self.get_value(thread, pointer)?
        };

        Ok(result)
    }

    pub fn extend(self, pool: &mut ObjectPool<'a>, values: &[(JsPrimitiveString, Value<'a>)]) {
        let obj = self.get_mut_object(pool);

        for (k, v) in values {
            obj.properties.insert(*k, Property::value(*v));
        }
    }

    pub(crate) fn get_property<'b>(
        self,
        pool: &'b ObjectPool<'a>,
        key: JsPrimitiveString,
    ) -> Option<&'b Property<'a>> {
        self.get_property_traverse(pool, key, false)
    }

    pub(crate) fn get_property_traverse<'b>(
        self,
        pool: &'b ObjectPool<'a>,
        key: JsPrimitiveString,
        own_properties_only: bool,
    ) -> Option<&'b Property<'a>> {
        let mut current = self;

        loop {
            let object = pool.get(current);

            if let Some(obj_property) = object.properties.get(&key) {
                return Some(obj_property);
            }

            if own_properties_only {
                return None;
            }

            if let Some(parent) = &object.prototype {
                current = *parent;
            } else {
                return None;
            }
        }
    }

    pub(crate) fn delete(self, pool: &mut ObjectPool<'a>, key: JsPrimitiveString) {
        let current = pool.get_mut(self);

        current.properties.remove(&key);
    }

    pub(crate) fn delete_indexed(self, pool: &mut ObjectPool<'a>, key: usize) {
        let current = pool.get_mut(self);

        current.indexed_properties.remove(key);
    }

    pub fn has(self, objects: &ObjectPool<'a>, key: JsPrimitiveString) -> bool {
        let object = objects.get(self);

        object.properties.contains_key(&key)
    }

    pub fn set(self, objects: &mut ObjectPool<'a>, key: JsPrimitiveString, value: Value<'a>) {
        let object = objects.get_mut(self);

        if let Some(Property::DataDescriptor {
            writable: false, ..
        }) = object.properties.get(&key)
        {
            return;
        }

        object.set(key, value)
    }

    pub fn set_indexed(self, thread: &mut JsThread<'a>, key: usize, value: Value<'a>) {
        let object = thread.realm.objects.get_mut(self);

        if key < 10000 {
            let indexed_properties = &mut object.indexed_properties;

            if indexed_properties.capacity() > 0 {
                match key.cmp(&indexed_properties.len()) {
                    Ordering::Less => indexed_properties[key] = value,
                    Ordering::Equal => indexed_properties.push(value),
                    Ordering::Greater => {
                        for _ in indexed_properties.len()..key {
                            indexed_properties.push(Value::UNDEFINED)
                        }
                        indexed_properties.push(value)
                    }
                }

                return;
            }
        }

        self.set(
            &mut thread.realm.objects,
            thread.realm.strings.intern(key.to_string()),
            value,
        )
    }

    pub fn define_value_property(
        self,
        pool: &mut ObjectPool<'a>,
        key: JsPrimitiveString,
        value: Value<'a>,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    ) {
        let object = pool.get_mut(self);

        object.define_value_property(key, value, writable, enumerable, configurable)
    }

    pub fn define_property(
        self,
        pool: &mut ObjectPool<'a>,
        key: JsPrimitiveString,
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
        enumerable: bool,
        configurable: bool,
    ) {
        let object = pool.get_mut(self);

        object.define_property(key, getter, setter, enumerable, configurable)
    }

    pub fn unwrap(self, pool: &ObjectPool<'a>) -> Option<Value<'a>> {
        let object = pool.get(self);

        object.get_wrapped_value()
    }

    pub fn wrap(self, thread: &mut JsThread<'a>, value: Value<'a>) {
        let object = thread.realm.objects.get_mut(self);

        object.set_wrapped_value(value);
    }

    pub fn call(self, thread: &mut JsThread<'a>, args: &[Value<'a>]) -> JsResult<'a, Value<'a>> {
        let function_reference = self
            .get_callable(&thread.realm.objects)
            .cloned()
            .ok_or_else(|| thread.new_type_error("is not a function".to_string()))?
            .clone();

        for arg in args {
            thread.push_stack(*arg);
        }

        thread.call_from_native(self.into(), function_reference, args.len(), false)
    }

    pub fn get_prototype(self, thread: &mut JsThread<'a>) -> Option<ObjectPointer<'a>> {
        thread.realm.objects.get(self).prototype()
    }

    pub fn get_name(self, pool: &ObjectPool<'a>) -> Option<JsPrimitiveString> {
        pool.get(self).name
    }

    pub fn get_construct<'b>(self, pool: &'b ObjectPool<'a>) -> Option<&'b FunctionReference<'a>> {
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
    pub fn get_callable<'b>(self, pool: &'b ObjectPool<'a>) -> Option<&'b FunctionReference<'a>> {
        let object = pool.get(self);

        object.callable.as_ref()
    }

    pub fn is_class_constructor(self, pool: &ObjectPool<'a>) -> bool {
        let object = pool.get(self);

        object.construct.is_some() && object.callable.is_none()
    }

    pub fn is_callable(self, pool: &ObjectPool<'a>) -> bool {
        let object = pool.get(self);

        object.construct.is_some() || object.callable.is_some()
    }
}
