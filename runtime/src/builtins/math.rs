use crate::debugging::DebugWithRealm;
use crate::result::JsResult;
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

    fn floor(thread: &mut JsThread<'a>, value1: Value<'a>) -> JsResult<'a> {
        let number: f64 = value1.to_number(&thread.realm)?;

        Ok(Value::from(number.floor()))
    }

    fn ceil(thread: &mut JsThread<'a>, value1: Value<'a>) -> JsResult<'a> {
        let number: f64 = value1.to_number(&thread.realm)?;

        Ok(Value::from(number.ceil()))
    }

    fn pow(thread: &mut JsThread<'a>, value1: Value<'a>, value2: Value<'a>) -> JsResult<'a> {
        let number: f64 = value1.to_number(&thread.realm)?;
        let number2: f64 = value2.to_number(&thread.realm)?;

        let result: Value = number.powf(number2).into();

        Ok(result)
    }

    fn round(thread: &mut JsThread<'a>, value1: Value<'a>) -> JsResult<'a> {
        let number: f64 = value1.to_number(&thread.realm)?;

        Ok(Value::from(number.round()))
    }

    fn trunc(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.trunc().into())
    }

    fn random(_: &mut JsThread<'a>) -> f64 {
        random()
    }

    fn sin(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.sin().into())
    }

    fn cos(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.cos().into())
    }

    fn tan(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.tan().into())
    }

    fn sinh(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.sinh().into())
    }

    fn cosh(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.cosh().into())
    }

    fn tanh(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.tanh().into())
    }

    fn sqrt(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.sqrt().into())
    }

    fn sign(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.signum().into())
    }

    fn log2(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.log2().into())
    }

    fn log10(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.log10().into())
    }

    fn log(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.ln().into())
    }

    fn log1p(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.ln_1p().into())
    }

    fn expm1(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.exp_m1().into())
    }

    fn exp(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.exp().into())
    }

    fn cbrt(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.cbrt().into())
    }

    fn atanh(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.atanh().into())
    }

    fn atan2(thread: &mut JsThread<'a>, value: Value<'a>, other: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;
        let other = other.to_number(&thread.realm)?;

        Ok(result.atan2(other).into())
    }

    fn acosh(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.acosh().into())
    }

    fn atan(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.atan().into())
    }

    fn asin(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.asin().into())
    }

    fn acos(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.acos().into())
    }

    fn abs(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.abs().into())
    }

    #[varargs]
    fn min(thread: &mut JsThread<'a>, args: Vec<Value<'a>>) -> JsResult<'a> {
        let mut min = f64::INFINITY;

        for value in &args {
            let value = value.to_number(&thread.realm)?;

            if value < min {
                min = value;
            }
        }

        Ok(min.into())
    }

    #[varargs]
    fn max(thread: &mut JsThread<'a>, args: Vec<Value<'a>>) -> JsResult<'a> {
        let mut min = f64::NEG_INFINITY;

        for value in &args {
            let value = value.to_number(&thread.realm)?;

            if value > min {
                min = value;
            }
        }

        Ok(min.into())
    }
}
