use crate::values::nan::Value;
use crate::JsThread;
use builtin::{named, prototype};
use rand::prelude::*;

pub(crate) struct JsMath<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Math")]
impl<'a, 'b> JsMath<'a, 'b> {
    fn floor(thread: &mut JsThread<'a>, value1: Value<'a>) -> Value<'a> {
        let number: f64 = value1.to_number(&thread.realm);

        Value::from(number.floor())
    }

    fn ceil(thread: &mut JsThread<'a>, value1: Value<'a>) -> Value<'a> {
        let number: f64 = value1.to_number(&thread.realm);

        Value::from(number.ceil())
    }

    fn round(thread: &mut JsThread<'a>, value1: Value<'a>) -> Value<'a> {
        let number: f64 = value1.to_number(&thread.realm);

        Value::from(number.round())
    }

    fn random(_: &mut JsThread<'a>) -> f64 {
        random()
    }
}
