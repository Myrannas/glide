use crate::{JsObject, JsThread};
use builtin::prototype;

pub(crate) struct JsFunctionObject<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsFunctionObject<'a, 'b> {}
