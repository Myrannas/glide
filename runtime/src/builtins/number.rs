use crate::values::nan::Value;
use crate::JsThread;
use builtin::{named, prototype};

#[allow(dead_code)]
pub(crate) struct JsNumber<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Number")]
impl<'a, 'b> JsNumber<'a, 'b> {
    #[named("parseInt")]
    fn parse_int(thread: &mut JsThread<'a>, value: Value<'a>) -> f64 {
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
}
