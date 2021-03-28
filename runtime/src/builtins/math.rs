use crate::debugging::DebugWithRealm;
use crate::values::nan::Value;
use crate::JsThread;
use builtin::{named, prototype, varargs};
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

    fn pow(thread: &mut JsThread<'a>, value1: Value<'a>, value2: Value<'a>) -> Value<'a> {
        let number: f64 = value1.to_number(&thread.realm);
        let number2: f64 = value2.to_number(&thread.realm);

        let result: Value = number.powf(number2).into();

        result
    }

    fn round(thread: &mut JsThread<'a>, value1: Value<'a>) -> Value<'a> {
        let number: f64 = value1.to_number(&thread.realm);

        Value::from(number.round())
    }

    fn trunc(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.trunc().into()
    }

    fn random(_: &mut JsThread<'a>) -> f64 {
        random()
    }

    fn sin(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.sin().into()
    }

    fn cos(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.cos().into()
    }

    fn tan(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.tan().into()
    }

    fn sinh(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.sinh().into()
    }

    fn cosh(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.cosh().into()
    }

    fn tanh(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.tanh().into()
    }

    fn sqrt(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.sqrt().into()
    }

    fn sign(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.signum().into()
    }

    fn log2(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.log2().into()
    }

    fn log10(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.log10().into()
    }

    fn log(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.ln().into()
    }

    fn log1p(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.ln_1p().into()
    }

    fn expm1(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.exp_m1().into()
    }

    fn exp(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.exp().into()
    }

    fn cbrt(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.cbrt().into()
    }

    fn atanh(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.atanh().into()
    }

    fn atan2(thread: &mut JsThread<'a>, value: Value<'a>, other: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);
        let other = other.to_number(&thread.realm);

        result.atan2(other).into()
    }

    fn acosh(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.acosh().into()
    }

    fn atan(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.atan().into()
    }

    fn asin(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.asin().into()
    }

    fn acos(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.acos().into()
    }

    fn abs(thread: &mut JsThread<'a>, value: Value<'a>) -> Value<'a> {
        let result = value.to_number(&thread.realm);

        result.abs().into()
    }

    #[varargs]
    fn min(thread: &mut JsThread<'a>, args: Vec<Value<'a>>) -> Value<'a> {
        let mut min = f64::INFINITY;

        for value in &args {
            let value = value.to_number(&thread.realm);

            if value < min {
                min = value;
            }
        }

        min.into()
    }

    #[varargs]
    fn max(thread: &mut JsThread<'a>, args: Vec<Value<'a>>) -> Value<'a> {
        let mut min = f64::NEG_INFINITY;

        for value in &args {
            let value = value.to_number(&thread.realm);

            if value > min {
                min = value;
            }
        }

        min.into()
    }
}
