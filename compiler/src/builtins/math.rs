use crate::{JsObject, JsThread, RuntimeValue};
use builtin::prototype;
use rand::prelude::*;

pub(crate) struct JsMath<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsMath<'a, 'b> {
    fn floor(value1: Option<&RuntimeValue<'a>>) -> f64 {
        let number: f64 = value1.unwrap_or_default().into();

        number.floor()
    }

    fn ceil(value1: Option<&RuntimeValue<'a>>) -> f64 {
        let number: f64 = value1.unwrap_or_default().into();

        number.ceil()
    }

    fn round(value1: Option<&RuntimeValue<'a>>) -> f64 {
        let number: f64 = value1.unwrap_or_default().into();

        number.round()
    }

    fn random() -> f64 {
        random()
    }
}
