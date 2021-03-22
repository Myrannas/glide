use crate::result::JsResult;
use crate::values::primitives::JsPrimitive;

use crate::{InternalError, JsThread, RuntimeValue};
use builtin::{callable, constructor, getter, named, prototype};

pub(crate) struct JsString<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("String")]
impl<'a, 'b> JsString<'a, 'b> {
    #[getter]
    fn length(&mut self) -> JsResult<'a> {
        let str = match self.target.to_object(self.thread)?.unwrap(self.thread) {
            Some(RuntimeValue::String(str)) => str,
            _ => {
                return InternalError::new_stackless(
                    "Attempted to call string length on non-string",
                )
                .into()
            }
        };

        let str = self.thread.realm.strings.get(str).as_ref();

        let count = str.chars().count();
        #[allow(clippy::cast_precision_loss)]
        Ok(RuntimeValue::Float(count as f64))
    }

    #[named("charAt")]
    fn char_at(&mut self, index: Option<RuntimeValue<'a>>) -> JsResult<'a> {
        let start_at: f64 = index
            .unwrap_or(RuntimeValue::Float(0.0))
            .to_number(&self.thread.realm);
        let end_at: RuntimeValue<'a> = (start_at + 1.0).into();
        let start_at: RuntimeValue<'a> = start_at.into();

        self.substring(Some(start_at), Some(end_at))
    }

    fn substring(
        &mut self,
        start_at: Option<RuntimeValue<'a>>,
        end_at: Option<RuntimeValue<'a>>,
    ) -> JsResult<'a> {
        let str = self
            .target
            .to_object(self.thread)?
            .unwrap(self.thread)
            .clone()
            .unwrap_or_default()
            .to_string(self.thread)?;

        let start_at = start_at
            .unwrap_or(RuntimeValue::Float(0.0))
            .to_usize(&self.thread.realm);
        let end_at = end_at
            .unwrap_or(RuntimeValue::Float(0.0))
            .to_usize(&self.thread.realm);

        let str = self.thread.realm.strings.get(str).as_ref();

        let chars = &str[start_at..end_at];

        Ok(RuntimeValue::String(
            self.thread.realm.strings.intern(chars.to_owned()),
        ))
    }

    #[callable]
    fn callable(thread: &mut JsThread<'a>, value: Option<RuntimeValue<'a>>) -> JsResult<'a> {
        let str = value
            .unwrap_or_else(|| thread.realm.constants.empty_string.into())
            .to_string(thread)?;

        Ok(RuntimeValue::String(str))
    }

    #[constructor]
    fn construct(&mut self, value: Option<RuntimeValue<'a>>) -> JsResult<'a, ()> {
        let str = value
            .unwrap_or_else(|| self.thread.realm.constants.empty_string.into())
            .to_string(self.thread)?;

        self.target
            .to_object(self.thread)?
            .wrap(self.thread, JsPrimitive::String(str));

        Ok(())
    }

    #[allow(clippy::unused_self)]
    #[named("localeCompare")]
    fn locale_compare(&self) {
        todo!("localeCompare is not yet supported")
    }
}
