use crate::{JsThread, Value};
use builtin::{constructor, named, prototype};

pub(crate) struct JsBoolean<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Boolean")]
impl<'a, 'b> JsBoolean<'a, 'b> {
    #[constructor]
    fn constructor(&mut self) {}
}
