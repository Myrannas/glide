use crate::object_pool::ObjectPointer;
use crate::{JsObject, JsThread, RuntimeValue};
use builtin::prototype;
use rand::prelude::*;

pub(crate) struct JsMath<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsMath<'a, 'b> {
    fn floor(value1: RuntimeValue<'a>) -> f64 {
        let number: f64 = value1.into();

        number.floor()
    }

    fn ceil(value1: RuntimeValue<'a>) -> f64 {
        let number: f64 = value1.into();

        number.ceil()
    }

    fn round(value1: RuntimeValue<'a>) -> f64 {
        let number: f64 = value1.into();

        number.round()
    }

    fn random() -> f64 {
        random()
    }
}
