use crate::debugging::{DebugRepresentation, Renderer};
use crate::pool::{Pool, PoolPointer};
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::values::nan::{Value, ValueType};
use crate::values::object::{Constructor, JsObjectState, Property, PropertyKey};
use crate::{ExecutionError, JsObject, JsPrimitiveString, JsThread};
use better_any::{TidAble, TidExt};
use stash::{Index, Stash};
use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::rc::Rc;

impl<'a> DebugRepresentation<'a> for ObjectPointer<'a> {
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        let obj = &renderer.realm.objects[*self];

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

pub type ObjectPool<'a> = Stash<JsObject<'a>, ObjectPointer<'a>>;

impl<'a> Index for ObjectPointer<'a> {
    fn from_usize(idx: usize) -> Self {
        ObjectPointer {
            inner: PoolPointer::from_usize(idx),
        }
    }

    fn into_usize(self) -> usize {
        self.inner.into_usize()
    }
}

impl<'a> ObjectPointer<'a> {
    pub(crate) fn get_object<'b>(self, pool: &'b ObjectPool<'a>) -> &'b JsObject<'a> {
        &pool[self]
    }

    pub(crate) fn get_mut_object<'b>(self, pool: &'b mut ObjectPool<'a>) -> &'b mut JsObject<'a> {
        &mut pool[self]
    }

    pub(crate) fn get_native_handle<'b, T: TidAble<'a>>(
        self,
        pool: &'b ObjectPool<'a>,
    ) -> Option<Ref<'b, T>> {
        match &pool[self].native_handle {
            Some(handle) => {
                let handle_ref = (*handle).as_ref().borrow();

                Ref::filter_map(handle_ref, |v| v.downcast_ref::<T>()).ok()
            }
            None => None,
        }
    }

    pub(crate) fn mut_native_handle<'b, T: TidAble<'a>>(
        self,
        pool: &'b mut ObjectPool<'a>,
    ) -> Option<RefMut<'b, &'a mut T>> {
        match &mut pool[self].native_handle {
            Some(handle) => {
                let handle_ref = (*handle).as_ref().borrow_mut();

                RefMut::filter_map(handle_ref, |v| v.downcast_mut::<T>()).ok();
                None
            }
            None => None,
        }
    }

    pub(crate) fn set_native_handle<'b, T: TidAble<'a>>(
        self,
        pool: &'b mut ObjectPool<'a>,
        value: Rc<RefCell<T>>,
    ) {
        pool[self].native_handle = Some(value)
    }

    pub(crate) fn set_prototype(self, pool: &mut ObjectPool<'a>, prototype: ObjectPointer<'a>) {
        pool[self].set_prototype(prototype)
    }

    pub fn set_construct(
        self,
        pool: &mut ObjectPool<'a>,
        construct: impl Into<FunctionReference<'a>>,
    ) {
        pool[self].set_construct(construct)
    }

    pub fn set_callable(
        self,
        pool: &mut ObjectPool<'a>,
        construct: impl Into<FunctionReference<'a>>,
    ) {
        pool[self].set_callable(construct)
    }

    pub fn get_value(
        self,
        thread: &mut JsThread<'a>,
        key: impl Into<PropertyKey>,
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
        let object = &thread.realm.objects[self];

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
            obj.properties
                .insert(PropertyKey::String(*k), Property::value(*v));
        }
    }

    pub(crate) fn get_property<'b>(
        self,
        pool: &'b ObjectPool<'a>,
        key: impl Into<PropertyKey>,
    ) -> Option<&'b Property<'a>> {
        self.get_property_traverse(pool, key, false)
    }

    pub(crate) fn get_property_traverse<'b>(
        self,
        pool: &'b ObjectPool<'a>,
        key: impl Into<PropertyKey>,
        own_properties_only: bool,
    ) -> Option<&'b Property<'a>> {
        let mut current = self;
        let property_key = key.into();

        loop {
            let object = &pool[current];
            if let Some(obj_property) = object.properties.get(&property_key) {
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

    pub(crate) fn delete(
        self,
        pool: &mut ObjectPool<'a>,
        key: impl Into<PropertyKey>,
    ) -> JsResult<'a, bool> {
        let current = &mut pool[self];

        let property_key = key.into();
        if let Some(existing) = current.properties.get(&property_key) {
            if existing.is_configurable() {
                current.properties.remove(&property_key);

                Ok(true)
            } else {
                Err(ExecutionError::TypeError(
                    "Property is not configurable".to_string(),
                ))
            }
        } else {
            Ok(true)
        }
    }

    pub(crate) fn delete_indexed(self, pool: &mut ObjectPool<'a>, key: usize) {
        let current = &mut pool[self];

        current.indexed_properties.remove(key);
    }

    pub fn has(self, objects: &ObjectPool<'a>, key: impl Into<PropertyKey>) -> bool {
        let object = &objects[self];

        object.properties.contains_key(&key.into())
    }

    pub fn set(
        self,
        thread: &mut JsThread<'a>,
        key: impl Into<PropertyKey>,
        value: Value<'a>,
    ) -> JsResult<'a, ()> {
        let property_key = key.into();

        let setter = {
            let object = &mut thread.realm.objects[self];
            let state = object.state;

            match object.properties.get_mut(&property_key) {
                Some(Property::DataDescriptor {
                    value: v,
                    writable: true,
                    ..
                }) => {
                    *v = value;
                    return Ok(());
                }
                Some(Property::AccessorDescriptor {
                    setter: Some(_), ..
                }) => true,
                None => {
                    return if matches!(state, JsObjectState::Extensible) {
                        Ok(object.set(property_key, value))
                    } else {
                        Err(ExecutionError::TypeError(
                            "Cannot add new properties".to_string(),
                        ))
                    }
                }
                _ => {
                    return Err(ExecutionError::TypeError(
                        "Property not writable".to_string(),
                    ))
                }
            }
        };

        if setter {
            let object = &thread.realm.objects[self];

            if let Some(Property::AccessorDescriptor {
                setter: Some(func), ..
            }) = object.properties.get(&property_key)
            {
                thread.call_from_native(Value::from(self), func.clone(), 1, false)?;
            }
        }
        Ok(())
    }

    pub fn set_ignore_setters(
        self,
        objects: &mut ObjectPool<'a>,
        key: JsPrimitiveString,
        value: Value<'a>,
    ) {
        let object = &mut objects[self];

        object.set(key, value);
    }

    pub fn set_indexed(
        self,
        thread: &mut JsThread<'a>,
        key: usize,
        value: Value<'a>,
    ) -> JsResult<'a, ()> {
        let object = &mut thread.realm.objects[self];

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

                return Ok(());
            }
        }

        let key = thread.realm.strings.intern(key.to_string());
        self.set(thread, key, value)
    }

    pub fn define_value_property(
        self,
        pool: &mut ObjectPool<'a>,
        key: JsPrimitiveString,
        value: Value<'a>,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    ) -> JsResult<'a, ()> {
        let object = &mut pool[self];

        if matches!(object.state, JsObjectState::Extensible) {
            Ok(object.define_value_property(key, value, writable, enumerable, configurable))
        } else {
            Err(ExecutionError::TypeError(
                "Cannot define property as object is not extensible".to_string(),
            ))
        }
    }

    pub fn define_property(
        self,
        pool: &mut ObjectPool<'a>,
        key: JsPrimitiveString,
        getter: Option<FunctionReference<'a>>,
        setter: Option<FunctionReference<'a>>,
        enumerable: bool,
        configurable: bool,
    ) -> JsResult<'a, ()> {
        let object = &mut pool[self];

        if matches!(object.state, JsObjectState::Extensible) {
            Ok(object.define_property(key, getter, setter, enumerable, configurable))
        } else {
            Err(ExecutionError::TypeError(
                "Cannot define property as object is not extensible".to_string(),
            ))
        }
    }

    pub fn unwrap(self, pool: &ObjectPool<'a>) -> Option<Value<'a>> {
        let object = &pool[self];

        object.get_wrapped_value()
    }

    pub fn wrap(self, thread: &mut JsThread<'a>, value: Value<'a>) {
        let object = &mut thread.realm.objects[self];

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
        thread.realm.objects[self].prototype()
    }

    pub fn get_name(self, pool: &ObjectPool<'a>) -> Option<JsPrimitiveString> {
        pool[self].name
    }

    pub fn get_construct<'b>(self, pool: &'b ObjectPool<'a>) -> Option<&'b Constructor<'a>> {
        let object = &pool[self];

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
        let object = &pool[self];

        object.callable.as_ref()
    }

    pub fn is_class_constructor(self, pool: &ObjectPool<'a>) -> bool {
        let object = &pool[self];

        object.construct.is_some() && object.callable.is_none()
    }

    pub fn is_callable(self, pool: &ObjectPool<'a>) -> bool {
        let object = &pool[self];

        object.construct.is_some() || object.callable.is_some()
    }
}
