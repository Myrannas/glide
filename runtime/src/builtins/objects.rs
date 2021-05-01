use crate::debugging::DebugWithRealm;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::nan::{Value, ValueType};
use crate::values::object::Property;
use crate::{JsObject, JsThread};
use builtin::{callable, constructor, named, prototype};

pub(crate) struct JsObjectBase<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Object")]
impl<'a, 'b> JsObjectBase<'a, 'b> {
    #[constructor]
    fn constructor(&mut self) {}

    #[named("getOwnPropertyDescriptor")]
    fn get_property_descriptor(
        thread: &mut JsThread<'a>,
        target: Value<'a>,
        property: Value<'a>,
    ) -> JsResult<'a, Value<'a>> {
        let string_key = property.to_string(thread)?;

        let object = thread.realm.objects.allocate(JsObject::new());

        match target
            .to_object(thread)?
            .get_property_traverse(&thread.realm.objects, string_key, true)
            .cloned()
        {
            Some(Property::DataDescriptor {
                value,
                enumerable,
                configurable,
                writable,
            }) => {
                object.extend(
                    &mut thread.realm.objects,
                    &[
                        (thread.realm.constants.enumerable, enumerable.into()),
                        (thread.realm.constants.configurable, configurable.into()),
                        (thread.realm.constants.writable, writable.into()),
                        (thread.realm.constants.value, value),
                    ],
                );
            }
            Some(Property::AccessorDescriptor {
                enumerable,
                configurable,
                ..
            }) => {
                object.extend(
                    &mut thread.realm.objects,
                    &[
                        (thread.realm.constants.enumerable, enumerable.into()),
                        (thread.realm.constants.configurable, configurable.into()),
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

    #[named("propertyIsEnumerable")]
    fn is_property_enumerable(&mut self, key: Value<'a>) -> JsResult<'a, bool> {
        let result = match key.get_type() {
            ValueType::String(str) => self
                .target
                .to_object(self.thread)?
                .get_property(&self.thread.realm.objects, str)
                .map_or(false, |property| match property {
                    Property::DataDescriptor { enumerable, .. }
                    | Property::AccessorDescriptor { enumerable, .. } => *enumerable,
                }),
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

        let value: Value = descriptor.get_value(self.thread, constants.value)?;

        let enumerable = match descriptor.get_value(self.thread, constants.enumerable)? {
            Value::UNDEFINED => true,
            other => other.to_bool(&self.thread.realm),
        };
        let writable = match descriptor.get_value(self.thread, constants.writable)? {
            Value::UNDEFINED => true,
            other => other.to_bool(&self.thread.realm),
        };
        let configurable = match descriptor.get_value(self.thread, constants.configurable)? {
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
