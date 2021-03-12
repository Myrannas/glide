use crate::{JsObject, JsThread};
use builtin::{callable, getter, named, prototype, varargs};

pub(crate) struct JsArguments<'a, 'b> {
    object: JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsArguments<'a, 'b> {
    #[getter]
    fn length(&self) -> f64 {
        self.object
            .get_indexed_properties()
            .as_ref()
            .map(|i| i.len())
            .unwrap_or(0) as f64
    }
}
