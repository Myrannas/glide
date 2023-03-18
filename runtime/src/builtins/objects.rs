use crate::debugging::X;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::nan::{Value, ValueType};
use crate::values::object::{JsObjectState, Property, PropertyKey};
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
                getter,
                setter,
            }) => {
                let getter: Value<'a> = getter
                    .map(|g| {
                        ValueType::Object(thread.realm.new_function_no_prototype(string_key, g))
                            .into()
                    })
                    .unwrap_or_default();

                let setter: Value<'a> = setter
                    .map(|s| {
                        ValueType::Object(thread.realm.new_function_no_prototype(string_key, s))
                            .into()
                    })
                    .unwrap_or_default();

                object.extend(
                    &mut thread.realm.objects,
                    &[
                        (thread.realm.constants.enumerable, enumerable.into()),
                        (thread.realm.constants.configurable, configurable.into()),
                        (thread.realm.constants.get, getter),
                        (thread.realm.constants.set, setter),
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
            .filter_map(|(l, _)| match l {
                PropertyKey::String(str) => Some(Value::from(*str)),
                PropertyKey::Symbol(_) => None,
            })
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

    fn freeze(&mut self, obj: Value<'a>) -> JsResult<'a> {
        let object = if let ValueType::Object(obj) = obj.get_type() {
            obj
        } else {
            return Err(self
                .thread
                .new_type_error("Object.seal called on non-object")
                .into());
        };

        let js_object = self.thread.realm.objects.get_mut(object);

        for (_, property) in &mut js_object.properties {
            property.set_writable(false);
        }

        self.seal(obj)
    }

    fn seal(&mut self, obj: Value<'a>) -> JsResult<'a> {
        let object = if let ValueType::Object(obj) = obj.get_type() {
            obj
        } else {
            return Err(self
                .thread
                .new_type_error("Object.seal called on non-object")
                .into());
        };

        let js_object = self.thread.realm.objects.get_mut(object);

        for (_, property) in &mut js_object.properties {
            property.set_configurable(false);
        }

        self.prevent_extensibility(obj)
    }

    #[named("preventExtensibility")]
    fn prevent_extensibility(&mut self, obj: Value<'a>) -> JsResult<'a> {
        let object = if let ValueType::Object(obj) = obj.get_type() {
            obj
        } else {
            return Err(self
                .thread
                .new_type_error("Object.preventExtensibility called on non-object")
                .into());
        };

        let js_object = self.thread.realm.objects.get_mut(object);

        js_object.state = JsObjectState::NotExtensible;

        Ok(Value::UNDEFINED)
    }

    #[named("isSealed")]
    fn is_sealed(&mut self, obj: Value<'a>) -> JsResult<'a, bool> {
        let object = if let ValueType::Object(obj) = obj.get_type() {
            obj
        } else {
            return Err(self
                .thread
                .new_type_error("Object.is_sealed called on non-object")
                .into());
        };

        let js_object = self.thread.realm.objects.get(object);

        let matches: bool = matches!(js_object.state, JsObjectState::NotExtensible);
        Ok(matches)
    }

    #[named("isFrozen")]
    fn is_frozen(&mut self, obj: Value<'a>) -> JsResult<'a, bool> {
        self.is_sealed(obj)
    }

    #[named("isExtensible")]
    fn is_extensible(&mut self, obj: Value<'a>) -> JsResult<'a, bool> {
        self.is_sealed(obj).map(|v| !v)
    }

    #[named("isPrototypeOf")]
    fn is_prototype_of(&mut self, obj: Value<'a>) -> JsResult<'a, bool> {
        let object = if let ValueType::Object(obj) = obj.get_type() {
            obj
        } else {
            return Err(self
                .thread
                .new_type_error("Object.isPrototypeOf called on non-object")
                .into());
        };
        let target = self.target.as_object(&self.thread.realm)?;

        if let Some(prototype) = object.get_prototype(self.thread) {
            Ok(prototype == target)
        } else {
            Ok(false)
        }
    }
}
