use crate::result::JsResult;
use crate::values::nan::Value;
use crate::JsThread;
use builtin::{getter, prototype};

pub(crate) struct JsArguments<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsArguments<'a, 'b> {
    #[getter]
    fn length(&mut self) -> JsResult<'a, f64> {
        let pointer = self.target.to_object(self.thread)?;

        #[allow(clippy::cast_precision_loss)]
        let length = pointer
            .get_object(&self.thread.realm.objects)
            .get_indexed_properties()
            .len() as f64;

        Ok(length)
    }
}
