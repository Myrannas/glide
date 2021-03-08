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
    fn parse_int(
        value: Option<&RuntimeValue<'a>>,
        radix: Option<&RuntimeValue<'a>>,
    ) -> JsResult<'a, f64> {
        let result: f64 = value.unwrap_or_default().into();

        Ok(result)
    }
}
