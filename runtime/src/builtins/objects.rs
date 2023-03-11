use crate::debugging::X;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::nan::{Value, ValueType};
use crate::values::object::Property;
use crate::{dv, JsObject, JsThread};
use builtin::{constructor, named, prototype};

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
            .to_object(&mut thread.realm)?
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

    #[named("keys")]
    fn keys(thread: &mut JsThread<'a>, key: Value<'a>) -> JsResult<'a> {
        let value = key.as_object(&thread.realm)?;

        let keys = thread
            .realm
            .get_object(value)
            .properties
            .iter()
            .filter(|(_, r)| r.is_enumerable())
            .map(|(l, _)| Value::from(*l))
            .collect();

        Ok(thread
            .realm
            .wrappers
            .wrap_array(&mut thread.realm.objects, keys)
            .into())
    }

    #[named("hasOwnProperty")]
    fn has_own_property(&mut self, key: Value<'a>) -> JsResult<'a, bool> {
        let result = match key.get_type() {
            ValueType::String(str) => self
                .target
                .to_object(&mut self.thread.realm)?
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
                .to_object(&mut self.thread.realm)?
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
            Value::UNDEFINED => false,
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

    #[named("toString")]
    fn to_string(&mut self) -> JsResult<'a> {
        Ok(ValueType::String(self.thread.realm.strings.intern_native("[object Object]")).into())
    }
}
