use crate::object_pool::ObjectPool;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::nan::{Value, ValueType};
use crate::values::object::Property;
use crate::{JsObject, JsThread};
use builtin::{named, prototype};

pub(crate) struct JsObjectBase<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Object")]
impl<'a, 'b> JsObjectBase<'a, 'b> {
    #[named("getOwnPropertyDescriptor")]
    fn get_property_descriptor(&mut self, property: Value<'a>) -> JsResult<'a, Value<'a>> {
        let string_key = property.to_string(self.thread)?;

        let object = self.thread.realm.objects.allocate(JsObject::new());

        match self
            .target
            .to_object(self.thread)?
            .get_property(&self.thread.realm.objects, string_key)
            .cloned()
        {
            Some(Property::DataDescriptor {
                value,
                enumerable,
                configurable,
                writable,
            }) => {
                object.extend(
                    &mut self.thread.realm.objects,
                    &[
                        (self.thread.realm.constants.enumerable, enumerable.into()),
                        (
                            self.thread.realm.constants.configurable,
                            configurable.into(),
                        ),
                        (self.thread.realm.constants.writable, writable.into()),
                        (self.thread.realm.constants.value, value.into()),
                    ],
                );
            }
            Some(Property::AccessorDescriptor {
                getter,
                setter,
                enumerable,
                configurable,
            }) => {
                object.extend(
                    &mut self.thread.realm.objects,
                    &[
                        (self.thread.realm.constants.enumerable, enumerable.into()),
                        (
                            self.thread.realm.constants.configurable,
                            configurable.into(),
                        ),
                    ],
                );
                // object.set("get");
                // object.set("set");
            }
            None => return Ok(Value::UNDEFINED),
        }

        Ok(object.into())
    }

    #[named("hasOwnProperty")]
    fn has_own_property(&mut self, key: Value<'a>) -> JsResult<'a, bool> {
        let result = match key.get_type() {
            ValueType::String(str) => self
                .target
                .to_object(self.thread)?
                .has(&self.thread.realm.objects, str),
            _ => false,
        };

        Ok(result)
    }

    #[named("defineProperty")]
    fn define_property(
        &mut self,
        obj: Value<'a>,
        key: Value<'a>,
        property_descriptor: Value<'a>,
    ) -> JsResult<'a, Value<'a>> {
        let object = if let ValueType::Object(obj) = obj.get_type() {
            obj
        } else {
            return Err(self
                .thread
                .new_type_error("Object.defineProperty called on non-object")
                .into());
        };

        let descriptor = if let ValueType::Object(obj) = property_descriptor.get_type() {
            obj
        } else {
            return Err(self
                .thread
                .new_type_error("Property description must be an object: {}")
                .into());
        };

        let constants = self.thread.realm.constants;

        let value: Value = descriptor.get_value(self.thread, constants.value)?.into();

        let enumerable = match descriptor
            .get_value(self.thread, constants.enumerable)?
            .into()
        {
            Value::UNDEFINED => true,
            other => other.to_bool(&self.thread.realm),
        };
        let writable = match descriptor
            .get_value(self.thread, constants.writable)?
            .into()
        {
            Value::UNDEFINED => true,
            other => other.to_bool(&self.thread.realm),
        };
        let configurable = match descriptor
            .get_value(self.thread, constants.configurable)?
            .into()
        {
            Value::UNDEFINED => true,
            other => other.to_bool(&self.thread.realm),
        };

        let key = key.to_string(self.thread)?;

        object.define_value_property(
            &mut self.thread.realm.objects,
            key,
            value,
            writable,
            enumerable,
            configurable,
        );

        Ok(Value::UNDEFINED)
    }
}
