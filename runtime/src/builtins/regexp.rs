use crate::{JsThread, Value};
use builtin::{constructor, prototype};

pub(crate) struct RegExp<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> RegExp<'a, 'b> {
    #[constructor]
    fn constructor(&mut self) {}
}
