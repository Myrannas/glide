use crate::builtins::native_target;
use crate::result::JsResult;
use crate::{JsThread, Value, ValueType};
use better_any::tid;
use builtin::{callable, constructor, named, prototype, varargs};
use chrono::{Datelike, TimeZone, Utc};
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) struct JsDate<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

struct NativeDate {
    date: f64,
}
tid!(NativeDate);

trait IntoValueTypes<'a> {
    fn value_types(&self) -> Vec<ValueType<'a>>;
}

impl<'a> IntoValueTypes<'a> for Vec<Value<'a>> {
    fn value_types(&self) -> Vec<ValueType<'a>> {
        self.iter()
            .map(|value| value.get_type())
            .collect::<Vec<ValueType<'a>>>()
    }
}

#[prototype]
#[named("Date")]
impl<'a, 'b> JsDate<'a, 'b> {
    #[varargs]
    #[constructor]
    fn constructor(&mut self, args: Vec<Value<'a>>) -> JsResult<'a, ()> {
        let mut date: f64 = match &*args {
            &[year, month] => {
                let mut date = Utc::now();

                date = date
                    .with_year(year.to_i32(&self.thread.realm)?)
                    .unwrap_or_default();
                date = date
                    .with_month(month.to_u32(&self.thread.realm)?)
                    .unwrap_or_default();

                date.timestamp_millis() as f64
            }
            &[timestamp] => timestamp.to_number(&self.thread.realm)? as f64,
            _ => Utc::now().timestamp_millis() as f64,
        };

        date = date.abs().floor().copysign(date);

        if date.is_infinite() {
            date = f64::NAN;
        }

        self.target
            .as_object(&mut self.thread.realm)?
            .set_native_handle(
                &mut self.thread.realm.objects,
                Rc::new(RefCell::new(NativeDate { date })),
            );

        Ok(())
    }

    #[callable]
    fn call(thread: &mut JsThread<'a>) -> JsResult<'a> {
        let str = Utc::now().to_rfc2822();

        Ok(thread.realm.strings.intern(str).into())
    }

    fn now(_: &'b mut JsThread<'a>) -> JsResult<'a> {
        let current_time = Utc::now();

        Ok((current_time.timestamp_millis() as f64).into())
    }

    #[named("valueOf")]
    fn value(&mut self) -> JsResult<'a> {
        let date = native_target::<NativeDate>(self.target, &self.thread.realm)?.date;

        Ok((date as f64).into())
    }

    #[named("toString")]
    fn to_string(&mut self) -> JsResult<'a> {
        let date = native_target::<NativeDate>(self.target, &self.thread.realm)?.date;

        let formatted_string = Utc
            .timestamp_millis_opt(date as i64)
            .single()
            .unwrap_or_default()
            .to_rfc2822();

        let interned = self.thread.realm.strings.intern(formatted_string);

        Ok(interned.into())
    }
}
