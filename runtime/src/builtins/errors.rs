use crate::result::JsResult;
use crate::values::value::RuntimeValue;
use crate::{JsObject, JsThread};
use builtin::{constructor, named, prototype};

pub(crate) struct JsError<'a, 'b> {
    object: JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: RuntimeValue<'a>) {
        self.object.define_value("message", message.clone());
    }

    #[named("toString")]
    fn as_string(&mut self) -> JsResult<'a> {
        let message = self.object.get("message".into(), self.thread)?;

        Ok(message.to_string(self.thread)?.into())
    }
}
