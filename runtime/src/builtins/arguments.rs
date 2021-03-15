use crate::object_pool::ObjectPointer;
use crate::{JsObject, JsThread};
use builtin::{callable, getter, named, prototype, varargs};

pub(crate) struct JsArguments<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsArguments<'a, 'b> {
    #[getter]
    fn length(&self) -> f64 {
        self.object
            .get_object(self.thread)
            .get_indexed_properties()
            .len() as f64
    }
}
