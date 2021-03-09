use crate::object::JsPrimitive;
use crate::result::JsResult;
use crate::string::JsPrimitiveString;
use crate::{InternalError, JsObject, JsThread, RuntimeValue};
use builtin::{callable, constructor, getter, named, prototype};
use std::rc::Rc;

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

        Ok(str.as_ref().len() as f64)
    }

    #[named("charAt")]
    fn char_at(&mut self, index: Option<RuntimeValue<'a>>) -> JsResult<'a> {
        let start_at: f64 = index.unwrap_or(RuntimeValue::Float(0.0)).into();
        let end_at: RuntimeValue<'a> = (start_at + 1.0).into();
        let start_at: RuntimeValue<'a> = start_at.into();

        self.substring(Some(start_at), Some(end_at))
    }

    fn substring(
        &mut self,
        start_at: Option<RuntimeValue<'a>>,
        end_at: Option<RuntimeValue<'a>>,
    ) -> JsResult<'a> {
        let string_value = self
            .object
            .get_wrapped_value()
            .clone()
            .unwrap_or_default()
            .to_string(self.thread)?;

        let start_at: f64 = start_at.unwrap_or(RuntimeValue::Float(0.0)).into();
        let end_at: f64 = end_at.unwrap_or(RuntimeValue::Float(0.0)).into();

        let chars = string_value.as_ref()[(start_at as usize)..(end_at as usize)].to_owned();

        Ok(RuntimeValue::String(chars.into()))
    }

    #[constructor]
    fn constructor(&mut self, value: RuntimeValue<'a>) -> JsResult<'a, ()> {
        let str = value.to_string(self.thread)?;

        self.object.wrap(JsPrimitive::String(str));

        Ok(())
    }

    #[callable]
    fn callable(thread: &'b mut JsThread<'a>, value: RuntimeValue<'a>) -> JsResult<'a> {
        let str = value.to_string(thread)?;

        Ok(RuntimeValue::String(str))
    }
}
