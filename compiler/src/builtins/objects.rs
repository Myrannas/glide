use crate::result::JsResult;
use crate::{JsObject, JsThread, RuntimeValue};
use builtin::{named, prototype};
use std::rc::Rc;

pub(crate) struct JsObjectBase<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsObjectBase<'a, 'b> {
    #[named("getOwnPropertyDescriptor")]
    fn get_property_descriptor(&mut self) -> JsResult<'a> {
        let object = JsObject::new();
        object.set(Rc::new("enumerable".to_owned()), false);
        Ok(object.into())
    }
}
