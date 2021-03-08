use crate::{JsObject, JsThread, RuntimeValue};
use builtin::{constructor, prototype, varargs};

pub(crate) struct JsArray<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsArray<'a, 'b> {
    #[varargs]
    #[constructor]
    fn constructor(&mut self, args: Vec<RuntimeValue<'a>>) {}
}
