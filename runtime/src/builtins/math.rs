use crate::{JsThread, RuntimeValue};
use builtin::{named, prototype};
use rand::prelude::*;

pub(crate) struct JsMath<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Math")]
impl<'a, 'b> JsMath<'a, 'b> {
    fn floor(thread: &mut JsThread<'a>, value1: RuntimeValue<'a>) -> RuntimeValue<'a> {
        let number: f64 = value1.to_number(&thread.realm);

        RuntimeValue::Float(number.floor())
    }

    fn ceil(thread: &mut JsThread<'a>, value1: RuntimeValue<'a>) -> RuntimeValue<'a> {
        let number: f64 = value1.to_number(&thread.realm);

        RuntimeValue::Float(number.ceil())
    }

    fn round(thread: &mut JsThread<'a>, value1: RuntimeValue<'a>) -> RuntimeValue<'a> {
        let number: f64 = value1.to_number(&thread.realm);

        RuntimeValue::Float(number.round())
    }

    fn random(_: &mut JsThread<'a>) -> f64 {
        random()
    }
}
