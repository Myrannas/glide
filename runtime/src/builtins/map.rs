use super::{native_target, native_target_mut};
use crate::result::JsResult;
use crate::values::nan::ValueType;
use crate::{JsThread, Unwrap, Value};
use better_any::tid;
use builtin::{constructor, getter, named, prototype};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub(crate) struct JsMap<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

struct NativeMap<'a>(HashMap<Value<'a>, Value<'a>>);
tid!(NativeMap<'a>);

#[prototype]
#[named("Map")]
impl<'a, 'b> JsMap<'a, 'b> {
    #[constructor]
    fn constructor(&mut self) {
        let map = HashMap::new();

        self.target
            .to_object(&mut self.thread.realm)
            .expect_value(
                self.thread.get_realm(),
                "Constructor must have object target",
            )
            .set_native_handle::<NativeMap>(
                &mut self.thread.realm.objects,
                Rc::new(RefCell::new(NativeMap(map))),
            );
    }

    fn set(&mut self, key: Value<'a>, value: Value<'a>) -> JsResult<'a> {
        native_target_mut::<NativeMap<'a>>(self.target, &mut self.thread.realm)?
            .0
            .insert(key, value);

        Ok(self.target)
    }

    #[named("forEach")]
    fn for_each(&mut self, function: Value<'a>) -> JsResult<'a, Option<Value<'a>>> {
        let items = {
            let elements = native_target::<NativeMap<'a>>(self.target, &self.thread.realm)?;
            elements.0.clone()
        };

        for (key, value) in &items {
            function.call(self.thread, &[*key, *value])?;
        }

        Ok(None)
    }

    fn clear(&mut self) -> JsResult<'a> {
        let mut elements = native_target_mut::<NativeMap<'a>>(self.target, &mut self.thread.realm)?;

        elements.0.clear();

        Ok(Value::UNDEFINED)
    }

    fn has(&mut self, value: Value<'a>) -> JsResult<'a, bool> {
        Ok(
            native_target::<NativeMap<'a>>(self.target, &self.thread.realm)?
                .0
                .contains_key(&value),
        )
    }

    fn keys(&mut self) {}

    fn delete(&mut self, value: Value<'a>) -> JsResult<'a, bool> {
        Ok(
            native_target_mut::<NativeMap<'a>>(self.target, &mut self.thread.realm)?
                .0
                .remove(&value)
                .is_some(),
        )
    }

    #[getter]
    fn size(&mut self) -> JsResult<'a, i32> {
        Ok(
            native_target::<NativeMap<'a>>(self.target, &self.thread.realm)?
                .0
                .len() as i32,
        )
    }
    fn entries(&mut self) -> JsResult<'a> {
        native_target::<NativeMap<'a>>(self.target, &self.thread.realm)?;

        Ok(Value::UNDEFINED)
    }
    fn values(&mut self) -> JsResult<'a> {
        native_target::<NativeMap<'a>>(self.target, &self.thread.realm)?;

        Ok(Value::UNDEFINED)
    }
}
