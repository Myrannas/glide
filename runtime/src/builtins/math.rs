use crate::object_pool::ObjectPool;
use crate::result::JsResult;
use crate::string_pool::StringPool;
use crate::values::nan::Value;
use crate::values::symbols::SymbolRegistry;
use crate::JsThread;
use builtin::{constant, named, prototype, varargs};
use rand::prelude::*;
use std::f64::consts::{E, PI};

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

    fn asinh(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)?;

        Ok(result.asinh().into())
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

    fn fround(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_number(&thread.realm)? as f32;

        Ok((result as f64).into())
    }

    fn imul(thread: &mut JsThread<'a>, value_a: Value<'a>, value_b: Value<'a>) -> JsResult<'a> {
        let a = value_a.to_i32(&thread.realm)?;
        let b = value_b.to_i32(&thread.realm)?;

        let result = a * b;

        Ok((result as f64).into())
    }

    #[varargs]
    fn hypot(thread: &mut JsThread<'a>, args: Vec<Value<'a>>) -> JsResult<'a> {
        let mut acc = f64::INFINITY;

        for value in &args {
            let value = value.to_number(&thread.realm)?;

            acc += value * value;
        }

        Ok(acc.sqrt().into())
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

    fn clz32(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let result = value.to_u32(&thread.realm)?;

        Ok((result.leading_zeros() as i32).into())
    }

    #[constant]
    #[named("E")]
    fn e(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        E.into()
    }

    #[constant]
    #[named("PI")]
    fn pi(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        PI.into()
    }

    #[constant]
    #[named("LN10")]
    fn ln10(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        10f64.ln().into()
    }

    #[constant]
    #[named("LN2")]
    fn ln2(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        2f64.ln().into()
    }

    #[constant]
    #[named("LOG10E")]
    fn log10e(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        E.log10().into()
    }

    #[constant]
    #[named("LOG2E")]
    fn log2e(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        E.log2().into()
    }

    #[constant]
    #[named("SQRT1_2")]
    fn sqrt1_2(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        (0.5f64).sqrt().into()
    }

    #[constant]
    #[named("SQRT2")]
    fn sqrt2(
        _pool: &mut ObjectPool<'a>,
        _strings: &mut StringPool,
        _symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        2f64.sqrt().into()
    }
}
