use crate::result::JsResult;
use crate::value::{BuiltIn, RuntimeValue};
use crate::{JsObject, JsThread};
use builtin::{named, prototype};
use std::rc::Rc;

pub(crate) struct JsError<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsError<'a, 'b> {
    fn constructor(&self, message: Option<&RuntimeValue<'a>>) {
        let message_as_string = message.unwrap_or_default();
        self.object
            .define_value("message", message_as_string.clone());
    }

    #[named("toString")]
    fn as_string(&mut self) -> JsResult<'a> {
        let message = self.object.get(Rc::new("message".into()), self.thread)?;

        Ok(message.to_string(self.thread)?.into())
    }
}