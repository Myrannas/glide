use crate::object_pool::ObjectPointer;
use crate::{JsObject, JsThread};
use builtin::{named, prototype};

pub(crate) struct JsFunctionObject<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Function")]
impl<'a, 'b> JsFunctionObject<'a, 'b> {}
