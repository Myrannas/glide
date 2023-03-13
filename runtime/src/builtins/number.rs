use crate::object_pool::ObjectPool;
use crate::result::JsResult;
use crate::string_pool::StringPool;
use crate::values::nan::Value;
use crate::values::symbols::SymbolRegistry;
use crate::{ExecutionError, JsThread, ValueType};
use builtin::{constant, named, prototype};

#[allow(dead_code)]
pub(crate) struct JsNumber<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Number")]
impl<'a, 'b> JsNumber<'a, 'b> {
    #[named("parseInt")]
    fn parse_int(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a, f64> {
        value.to_number(&thread.realm)
    }

    #[named("isFinite")]
    fn is_finite(_: &mut JsThread<'a>, value: Value<'a>) -> bool {
        value.float().is_finite()
    }

    #[named("isNaN")]
    fn is_nan(_: &mut JsThread<'a>, value: Value<'a>) -> bool {
        value.float().is_nan()
    }

    #[named("isSafeInteger")]
    fn is_safe_integer(_: &mut JsThread<'a>, value: Value<'a>) -> bool {
        let absolute_value = value.float().abs() as u64;

        absolute_value < ((2_u64 << 53) - 1)
    }

    #[named("toString")]
    fn to_string(&mut self) -> JsResult<'a> {
        match self.target.get_type() {
            ValueType::Float => Ok(ValueType::String(
                self.thread
                    .realm
                    .strings
                    .intern(format!("{}", self.target.float())),
            )
            .into()),
            _ => Err(ExecutionError::TypeError(
                "Number.prototype.toString requires that 'this' be a Number".to_string(),
            )),
        }
    }

    #[constant]
    #[named("NaN")]
    fn nan(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        Value::NAN
    }

    #[constant]
    #[named("NEGATIVE_INFINITY")]
    fn negative_infinity(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        f64::NEG_INFINITY.into()
    }

    #[constant]
    #[named("POSITIVE_INFINITY")]
    fn positive_infinity(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        f64::INFINITY.into()
    }

    #[constant]
    #[named("MIN_VALUE")]
    fn min_value(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        f64::MIN_POSITIVE.into()
    }

    #[constant]
    #[named("MAX_VALUE")]
    fn max_value(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        f64::MAX.into()
    }

    #[constant]
    #[named("EPSILON")]
    fn epsilon(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        f64::EPSILON.into()
    }

    #[constant]
    #[named("MAX_SAFE_INTEGER")]
    fn max_safe_integer(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        ((2i64 << 53 - 1) as f64).into()
    }

    #[constant]
    #[named("MIN_SAFE_INTEGER")]
    fn min_safe_integer(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        (-(2i64 << 53 - 1) as f64).into()
    }
}
