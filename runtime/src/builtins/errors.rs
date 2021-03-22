use crate::result::JsResult;
use crate::values::value::RuntimeValue;
use crate::JsThread;
use builtin::{constructor, named, prototype};

pub(crate) struct JsError<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Error")]
impl<'a, 'b> JsError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: &RuntimeValue<'a>) {
        self.target
            .to_object(self.thread)
            .expect("Constructor must have an object target")
            .set(
                &mut self.thread.realm.objects,
                self.thread.realm.constants.message,
                message.clone(),
            );
    }

    #[named("toString")]
    fn as_string(&mut self) -> JsResult<'a> {
        let message = self
            .target
            .to_object(self.thread)?
            .get_value(self.thread, self.thread.realm.constants.message)?;

        Ok(message.to_string(self.thread)?.into())
    }
}

pub(crate) struct TypeError<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("TypeError")]
impl<'a, 'b> TypeError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: &RuntimeValue<'a>) {
        self.target
            .to_object(self.thread)
            .expect("Constructor must have an object target")
            .set(
                &mut self.thread.realm.objects,
                self.thread.realm.constants.message,
                message.clone(),
            );
    }
}

pub(crate) struct ReferenceError<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("ReferenceError")]
impl<'a, 'b> ReferenceError<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, message: &RuntimeValue<'a>) {
        self.target
            .to_object(self.thread)
            .expect("Constructor must have an object target")
            .set(
                &mut self.thread.realm.objects,
                self.thread.realm.constants.message,
                message.clone(),
            );
    }
}
