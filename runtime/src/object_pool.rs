use crate::debugging::{DebugRepresentation, Renderer};
use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::values::object::Property;
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
            }) => thread.call_from_native(self.clone(), function, 0, false)?,
            _ => RuntimeValue::Undefined,
        };

        Ok(result)
    }

    pub fn get_indexed(
        &self,
        thread: &mut JsThread<'a>,
        key: usize,
    ) -> JsResult<'a, RuntimeValue<'a>> {
        todo!("get_indexed is not supported at present")
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

    pub fn as_function(&self, thread: &mut JsThread<'a>) -> Option<impl FunctionObject<'a>> {
        let object = thread.realm.objects.get(self);

        object.function()
    }

    pub fn call(&self, thread: &mut JsThread<'a>, args: &[RuntimeValue<'a>]) -> JsResult<'a> {
        let function = self.as_function(thread);

        let function_reference = function.ok_or_else(|| {
            thread
                .realm
                .errors
                .new_type_error(&mut thread.realm.objects, "")
        })?;

        let function_reference = function_reference.callable().cloned().unwrap();

        for arg in args {
            thread.push_stack(arg.clone());
        }

        thread.call_from_native(self.clone(), function_reference, args.len(), false)
    }

    pub fn get_prototype(&self, thread: &mut JsThread<'a>) -> Option<ObjectPointer<'a>> {
        thread.get(self).prototype()
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
            objects: Vec::with_capacity(1024),
        }
    }
}

impl<'a> ObjectPool<'a> for JsObjectPool<'a> {
    fn get(&self, index: &ObjectPointer<'a>) -> &JsObject<'a> {
        &self.objects[index.index as usize]
    }

    fn get_mut(&mut self, index: &ObjectPointer<'a>) -> &mut JsObject<'a> {
        &mut self.objects[index.index as usize]
    }

    fn allocate(&mut self, object: JsObject<'a>) -> ObjectPointer<'a> {
        let index = self.objects.len();
        self.objects.push(object);

        ObjectPointer {
            index: index as u32,
            phantom_data: PhantomData,
        }
    }
}
