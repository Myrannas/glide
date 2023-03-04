use crate::debugging::Unwrap;
use crate::result::JsResult;
use crate::values::nan::Value;
use crate::JsThread;
use builtin::{constructor, named, prototype};

pub(crate) struct JsError<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Error")]
impl<'a, 'b> JsError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: Value<'a>) {
        self.target
            .to_object(&mut self.thread.realm)
            .expect_value(
                self.thread.get_realm(),
                "Constructor must have an object target",
            )
            .set(
                &mut self.thread.realm.objects,
                self.thread.realm.constants.message,
                message,
            );
    }

    #[named("toString")]
    fn as_string(&mut self) -> JsResult<'a> {
        let message = self
            .target
            .to_object(&mut self.thread.realm)?
            .get_value(self.thread, self.thread.realm.constants.message)?;

        Ok(message.to_string(self.thread)?.into())
    }
}

pub(crate) struct TypeError<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("TypeError")]
impl<'a, 'b> TypeError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: Value<'a>) {
        self.target
            .to_object(&mut self.thread.realm)
            .expect_value(
                self.thread.get_realm(),
                "Constructor must have an object target",
            )
            .set(
                &mut self.thread.realm.objects,
                self.thread.realm.constants.message,
                message,
            );
    }
}

pub(crate) struct ReferenceError<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("ReferenceError")]
impl<'a, 'b> ReferenceError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: Value<'a>) {
        self.target
            .to_object(&mut self.thread.realm)
            .expect_value(
                self.thread.get_realm(),
                "Constructor must have an object target",
            )
            .set(
                &mut self.thread.realm.objects,
                self.thread.realm.constants.message,
                message,
            );
    }
}
