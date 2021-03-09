use crate::result::JsResult;
use crate::{JsObject, JsThread, RuntimeValue};
use builtin::{named, prototype};

pub(crate) struct JsNumber<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsNumber<'a, 'b> {
    #[named("parseInt")]
    fn parse_int(value: RuntimeValue<'a>, radix: RuntimeValue<'a>) -> JsResult<'a, f64> {
        Ok(value.into())
    }

    #[named("isFinite")]
    fn is_finite(value: RuntimeValue<'a>) -> bool {
        match value {
            RuntimeValue::Float(value) => value.is_finite(),
            _ => false,
        }
    }

    #[named("isNaN")]
    fn is_nan(value: RuntimeValue<'a>) -> bool {
        match value {
            RuntimeValue::Float(value) => {
                if value.is_infinite() {
                    return false;
                }

                value.is_nan()
            }
            _ => false,
        }
    }

    #[named("isSafeInteger")]
    fn is_safe_integer(value: RuntimeValue<'a>) -> bool {
        match value {
            RuntimeValue::Float(value) => {
                let absolute_value = value.abs() as u64;

                absolute_value < (((2 as u64) << 53) - 1)
            }
            _ => false,
        }
    }
}
