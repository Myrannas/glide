use crate::values::nan::Value;
use crate::JsThread;
use builtin::{named, prototype};
use rand::prelude::*;

// #[allow(dead_code)]
pub(crate) struct JsMath {}

#[prototype]
#[named("Math")]
impl<'a, 'b> JsMath {
    #[allow(dead_code)]
    fn new(_: crate::values::nan::Value<'a>, _: &'b mut JsThread<'a>) -> Self {
        Self {}
    }

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
