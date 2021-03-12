use crate::result::JsResult;
use crate::values::object::Property;
use crate::{JsObject, JsThread, RuntimeValue};
use builtin::{named, prototype};

pub(crate) struct JsObjectBase<'a, 'b> {
    object: JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsObjectBase<'a, 'b> {
    #[named("getOwnPropertyDescriptor")]
    fn get_property_descriptor(&mut self, property: RuntimeValue<'a>) -> JsResult<'a> {
        let string_key = property.to_string(self.thread)?;

        let object = JsObject::new();

        match self.object.get_property(&string_key) {
            Some(Property::DataDescriptor {
                value,
                enumerable,
                configurable,
                writable,
            }) => {
                object.set("enumerable".into(), enumerable);
                object.set("configurable".into(), configurable);
                object.set("writable".into(), writable);
                object.set("value".into(), value);
            }
            Some(Property::AccessorDescriptor {
                getter,
                setter,
                enumerable,
                configurable,
            }) => {
                object.set("enumerable".into(), enumerable);
                object.set("configurable".into(), configurable);
                // object.set("get");
                // object.set("set");
            }
            None => return Ok(RuntimeValue::Undefined),
        }

        Ok(object.into())
    }

    #[named("hasOwnProperty")]
    fn has_own_property(&mut self, key: RuntimeValue<'a>) -> bool {
        match key {
            RuntimeValue::String(str) => self.object.has(str),
            _ => false,
        }
    }

    #[named("defineProperty")]
    fn define_property(
        &mut self,
        obj: RuntimeValue<'a>,
        key: RuntimeValue<'a>,
        property_descriptor: RuntimeValue<'a>,
    ) -> JsResult<'a> {
        let object = if let RuntimeValue::Object(obj) = obj {
            obj
        } else {
            return Err(self
                .thread
                .global_this
                .errors
                .new_type_error("Object.defineProperty called on non-object")
                .into());
        };

        let descriptor = if let RuntimeValue::Object(obj) = property_descriptor {
            obj
        } else {
            return Err(self
                .thread
                .global_this
                .errors
                .new_type_error("Property description must be an object: {}")
                .into());
        };

        let value: RuntimeValue = descriptor.get("value".into(), self.thread)?;
        let enumerable = match descriptor.get("enumerable".into(), self.thread)? {
            RuntimeValue::Undefined => true,
            other => other.into(),
        };
        let writable = match descriptor.get("writable".into(), self.thread)? {
            RuntimeValue::Undefined => true,
            other => other.into(),
        };
        let configurable = match descriptor.get("configurable".into(), self.thread)? {
            RuntimeValue::Undefined => true,
            other => other.into(),
        };

        let key = key.to_string(self.thread)?;

        object.define_value_property(key, value, writable, enumerable, configurable);

        Ok(RuntimeValue::Undefined)
    }
}
