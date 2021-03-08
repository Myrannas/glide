use crate::object::JsPrimitive;
use crate::result::JsResult;
use crate::{InternalError, JsObject, JsThread, RuntimeValue};
use builtin::{callable, getter, prototype};

pub(crate) struct JsString<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsString<'a, 'b> {
    #[getter]
    fn length(&mut self) -> JsResult<'a, f64> {
        let str = match self.object.get_wrapped_value() {
            Some(RuntimeValue::String(str)) => str,
            _ => {
                return InternalError::new_stackless(
                    "Attempted to call string length on non-string",
                )
                .into()
            }
        };

        Ok(str.len() as f64)
    }

    fn constructor(&mut self, value: Option<&RuntimeValue<'a>>) -> JsResult<'a, ()> {
        let value = value.unwrap_or_default();
        let str = value.to_string(self.thread)?;

        self.object.wrap(JsPrimitive::String(str));

        Ok(())
    }

    #[callable]
    fn callable(thread: &'b mut JsThread<'a>, value: Option<&RuntimeValue<'a>>) -> JsResult<'a> {
        let value = value.unwrap_or_default();
        let str = value.to_string(thread)?;

        Ok(RuntimeValue::String(str))
    }
}
