use crate::{JsThread, RuntimeValue};
use builtin::{named, prototype};

#[allow(dead_code)]
pub(crate) struct JsNumber<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Number")]
impl<'a, 'b> JsNumber<'a, 'b> {
    #[named("parseInt")]
    fn parse_int(thread: &mut JsThread<'a>, value: &RuntimeValue<'a>) -> f64 {
        value.to_number(&thread.realm)
    }

    #[named("isFinite")]
    fn is_finite(_: &mut JsThread<'a>, value: &RuntimeValue<'a>) -> bool {
        match value {
            RuntimeValue::Float(value) => value.is_finite(),
            _ => false,
        }
    }

    #[named("isNaN")]
    fn is_nan(_: &mut JsThread<'a>, value: &RuntimeValue<'a>) -> bool {
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
    fn is_safe_integer(_: &mut JsThread<'a>, value: &RuntimeValue<'a>) -> bool {
        match value {
            RuntimeValue::Float(value) => {
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let absolute_value = value.abs() as u64;

                absolute_value < ((2_u64 << 53) - 1)
            }
            _ => false,
        }
    }
}
