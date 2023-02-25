use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::{JsThread, Unwrap, Value};
use builtin::{constructor, named, prototype};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

pub(crate) struct JsSet<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

type NativeSet<'a> = HashSet<Value<'a>>;

#[prototype]
#[named("Set")]
impl<'a, 'b> JsSet<'a, 'b> {
    #[constructor]
    fn constructor(&mut self) {
        self.target
            .to_object(self.thread)
            .expect_value(
                self.thread.get_realm(),
                "Constructor must have object target",
            )
            .set_native_handle::<NativeSet>(
                &mut self.thread.realm.objects,
                Rc::new(RefCell::new(NativeSet::new())),
            )
    }

    fn add(&mut self, value: Value<'a>) -> JsResult<'a> {
        {
            let mut native_set = self
                .target
                .to_object(self.thread)
                .expect_value(
                    self.thread.get_realm(),
                    "Constructor must have object target",
                )
                .mut_native_handle::<NativeSet<'a>>(&mut self.thread.realm.objects);

            if let Some(set) = &mut native_set {
                set.insert(value);
                return Ok(Value::UNDEFINED);
            }
        }

        Err(self
            .thread
            .realm
            .new_type_error("Object is invalid target")
            .into())
    }

    #[named("forEach")]
    fn for_each(&mut self) {}

    fn clear(&mut self) {}
    fn has(&mut self) {}
    fn keys(&mut self) {}
    fn delete(&mut self) {}
    fn size(&mut self) {}
    fn entries(&mut self) {}
    fn values(&mut self) {}
}
