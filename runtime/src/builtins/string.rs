use crate::result::JsResult;

use crate::primordials::RuntimeHelpers;
use crate::values::nan::{Value, ValueType};
use crate::InternalError;
use crate::JsThread;
use builtin::{callable, constructor, getter, named, prototype};

pub(crate) struct JsString<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("String")]
impl<'a, 'b> JsString<'a, 'b> {
    #[getter]
    fn length(&mut self) -> JsResult<'a, Value<'a>> {
        let unwrapped_result: Value = self
            .target
            .to_object(&mut self.thread.realm)?
            .unwrap(&self.thread.realm.objects)
            .into();

        let str = match unwrapped_result.get_type() {
            ValueType::String(str) => str,
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
        Ok((count as f64).into())
    }

    #[named("charAt")]
    fn char_at(&mut self, index: Option<Value<'a>>) -> JsResult<'a, Value<'a>> {
        let start_at: f64 = index.unwrap_or(Value::ZERO).to_number(&self.thread.realm)?;
        let end_at = (start_at + 1.0).into();
        let start_at = start_at.into();

        self.substring(Some(start_at), Some(end_at))
    }

    fn substring(
        &mut self,
        start_at: Option<Value<'a>>,
        end_at: Option<Value<'a>>,
    ) -> JsResult<'a, Value<'a>> {
        let str = self
            .target
            .to_object(&mut self.thread.realm)?
            .unwrap(&self.thread.realm.objects)
            .clone()
            .unwrap_or_default()
            .to_string(self.thread)?;

        let start_at = start_at
            .unwrap_or(Value::ZERO)
            .to_usize(&self.thread.realm)?;
        let end_at = end_at.unwrap_or(Value::ZERO).to_usize(&self.thread.realm)?;

        let str = self.thread.realm.strings.get(str).as_ref();

        let chars = str[start_at..end_at].to_owned();

        Ok(ValueType::String(self.thread.realm.strings.intern(chars)).into())
    }

    fn slice(
        &mut self,
        start_at: Option<Value<'a>>,
        end_at: Option<Value<'a>>,
    ) -> JsResult<'a, Value<'a>> {
        let str = self
            .target
            .to_object(&mut self.thread.realm)?
            .unwrap(&self.thread.realm.objects)
            .clone()
            .unwrap_or_default()
            .to_string(self.thread)?;

        let start_at = start_at
            .unwrap_or(Value::ZERO)
            .to_usize(&self.thread.realm)?;
        let end_at = end_at.unwrap_or(Value::ZERO).to_usize(&self.thread.realm)?;

        let str = self.thread.realm.strings.get(str).as_ref();

        let chars = str[start_at..end_at].to_owned();

        Ok(ValueType::String(self.thread.realm.strings.intern(chars)).into())
    }

    #[callable]
    fn callable(thread: &mut JsThread<'a>, value: Option<Value<'a>>) -> JsResult<'a, Value<'a>> {
        let str = value
            .unwrap_or_else(|| thread.realm.constants.empty_string.into())
            .to_string(thread)?;

        Ok(ValueType::String(str).into())
    }

    #[constructor]
    fn construct(&mut self, value: Option<Value<'a>>) -> JsResult<'a, ()> {
        let str = value
            .unwrap_or_else(|| self.thread.realm.constants.empty_string.into())
            .to_string(self.thread)?;

        self.target
            .to_object(&mut self.thread.realm)?
            .wrap(self.thread, Value::from(str));

        Ok(())
    }

    fn trim(&mut self) -> JsResult<'a> {
        if self.target == Value::UNDEFINED || self.target == Value::NULL {
            return Err(self
                .thread
                .new_type_error("Can't call trim on undefined or null")
                .into());
        }

        let str = self.target.to_string(&mut self.thread)?;

        let str_value = self
            .thread
            .realm
            .strings
            .manipulate(str, |result| result.trim());

        Ok(str_value.into())
    }

    #[named("trimStart")]
    fn trim_start(&mut self) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;

        let str_value = self
            .thread
            .realm
            .strings
            .manipulate(str, |result| result.trim_start());

        Ok(str_value.into())
    }

    #[named("trimEnd")]
    fn trim_end(&mut self) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;

        let str_value = self
            .thread
            .realm
            .strings
            .manipulate(str, |result| result.trim_end());

        Ok(str_value.into())
    }

    #[named("toUpperCase")]
    fn to_upper_case(&mut self) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;

        let str_value = self
            .thread
            .realm
            .strings
            .manipulate_owned(str, str::to_uppercase);

        Ok(str_value.into())
    }

    #[named("toLowerCase")]
    fn to_lower_case(&mut self) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;

        let str_value = self
            .thread
            .realm
            .strings
            .manipulate_owned(str, str::to_lowercase);

        Ok(str_value.into())
    }

    #[named("toLocaleUpperCase")]
    fn to_locale_upper_case(&mut self) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;

        let str_value = self
            .thread
            .realm
            .strings
            .manipulate_owned(str, str::to_uppercase);

        Ok(str_value.into())
    }

    #[named("toLocaleLowerCase")]
    fn to_locale_lower_case(&mut self) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;

        let str_value = self
            .thread
            .realm
            .strings
            .manipulate_owned(str, str::to_lowercase);

        Ok(str_value.into())
    }

    #[named("toString")]
    fn to_string(&mut self) -> JsResult<'a> {
        let str = self
            .target
            .to_object(&mut self.thread.realm)?
            .unwrap(&self.thread.realm.objects);

        if let Some(str) = str {
            let result = str.to_string(&mut self.thread)?;

            Ok(result.into())
        } else {
            Err(self
                .thread
                .new_type_error("Can't call String.toString on non-string value")
                .into())
        }
    }

    #[named("startsWith")]
    fn starts_with(&mut self, input: Value<'a>) -> JsResult<'a> {
        if self.target == Value::UNDEFINED || self.target == Value::NULL {
            return Err(self
                .thread
                .new_type_error("Can't call starts_with on undefined or null")
                .into());
        }

        let str = self.target.to_string(&mut self.thread)?;
        let input = input.to_string(&mut self.thread)?;

        let str_value = self.thread.realm.strings.get(str);

        let input_value = self.thread.realm.strings.get(input);

        Ok(str_value.as_ref().starts_with(input_value.as_ref()).into())
    }

    #[named("endsWith")]
    fn ends_with(&mut self, input: Value<'a>) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;
        let input = input.to_string(&mut self.thread)?;

        let str_value = self.thread.realm.strings.get(str);

        let input_value = self.thread.realm.strings.get(input);

        Ok(str_value.as_ref().ends_with(input_value.as_ref()).into())
    }

    #[named("split")]
    fn split(&mut self, input: Value<'a>) -> JsResult<'a> {
        let str = self.target.to_string(&mut self.thread)?;
        let input = input.to_string(&mut self.thread)?;

        let str_value = self.thread.realm.strings.get(str);

        let input_value = self.thread.realm.strings.get(input);

        let strs: Vec<Value> = str_value
            .as_ref()
            .split(input_value.as_ref())
            .map(|v| self.thread.realm.strings.to_owned().intern(v))
            .map(Value::from)
            .collect();

        Ok(self
            .thread
            .realm
            .wrappers
            .wrap_array(&mut self.thread.realm.objects, strs)
            .into())
    }

    #[allow(clippy::unused_self)]
    #[named("localeCompare")]
    fn locale_compare(&self) {
        todo!("localeCompare is not yet supported")
    }
}
