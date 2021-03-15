use crate::object_pool::ObjectPointer;
use crate::result::JsResult;
use crate::values::value::RuntimeValue;
use crate::{JsObject, JsThread};
use builtin::{constructor, named, prototype};

pub(crate) struct JsError<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Error")]
impl<'a, 'b> JsError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: RuntimeValue<'a>) {
        self.object
            .set(self.thread, &"message".into(), message.clone());
    }

    #[named("toString")]
    fn as_string(&mut self) -> JsResult<'a> {
        let message = self.object.get_value(self.thread, &"message".into())?;

        Ok(message.to_string(self.thread)?.into())
    }
}

pub(crate) struct TypeError<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> TypeError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: RuntimeValue<'a>) {
        self.object
            .set(self.thread, &"message".into(), message.clone());
    }
}

pub(crate) struct ReferenceError<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> ReferenceError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: RuntimeValue<'a>) {
        self.object
            .set(self.thread, &"message".into(), message.clone());
    }
}
