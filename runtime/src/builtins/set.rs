use crate::builtins::{native_target, native_target_mut};
use crate::result::JsResult;
use crate::{ExecutionError, JsThread, Unwrap, Value};
use better_any::{tid, TidAble};
use builtin::{constructor, getter, named, prototype};
use std::cell::{RefCell, RefMut};
use std::collections::HashSet;
use std::rc::Rc;

pub(crate) struct JsSet<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

struct NativeSet<'a>(HashSet<Value<'a>>);
tid!(NativeSet<'a>);

#[prototype]
#[named("Set")]
impl<'a, 'b> JsSet<'a, 'b> {
    #[constructor]
    fn constructor(&mut self) {
        self.target
            .to_object(&mut self.thread.realm)
            .expect_value(&self.thread.realm, "Constructor must have object target")
            .set_native_handle::<NativeSet>(
                &mut self.thread.realm.objects,
                Rc::new(RefCell::new(NativeSet(HashSet::new()))),
            )
    }

    fn add(&mut self, value: Value<'a>) -> JsResult<'a> {
        native_target_mut::<NativeSet<'a>>(self.target, &mut self.thread.realm)?
            .0
            .insert(value);

        Ok(self.target)
    }

    #[named("forEach")]
    fn for_each(&mut self, function: Value<'a>) -> JsResult<'a, Option<Value<'a>>> {
        let items = {
            let elements = native_target::<NativeSet<'a>>(self.target, &self.thread.realm)?;
            elements.0.clone()
        };

        for element in &items {
            function.call(self.thread, &[*element])?;
        }

        Ok(None)
    }

    fn clear(&mut self) -> JsResult<'a> {
        let mut elements = native_target_mut::<NativeSet<'a>>(self.target, &mut self.thread.realm)?;

        elements.0.clear();

        Ok(Value::UNDEFINED)
    }

    fn has(&mut self, value: Value<'a>) -> JsResult<'a, bool> {
        Ok(
            native_target::<NativeSet<'a>>(self.target, &self.thread.realm)?
                .0
                .contains(&value),
        )
    }

    fn keys(&mut self) {}

    fn delete(&mut self, value: Value<'a>) -> JsResult<'a, bool> {
        Ok(
            native_target_mut::<NativeSet<'a>>(self.target, &mut self.thread.realm)?
                .0
                .remove(&value),
        )
    }

    #[getter]
    fn size(&mut self) -> JsResult<'a, i32> {
        Ok(
            native_target::<NativeSet<'a>>(self.target, &self.thread.realm)?
                .0
                .len() as i32,
        )
    }
    fn entries(&mut self) -> JsResult<'a> {
        native_target::<NativeSet<'a>>(self.target, &self.thread.realm)?;

        Ok(Value::UNDEFINED)
    }
    fn values(&mut self) -> JsResult<'a> {
        native_target::<NativeSet<'a>>(self.target, &self.thread.realm)?;

        Ok(Value::UNDEFINED)
    }
}
