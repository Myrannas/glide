use crate::result::JsResult;
use crate::{JsObject, JsThread, RuntimeValue};
use builtin::{callable, constructor, named, prototype, varargs};

pub(crate) struct JsArray<'a, 'b> {
    object: &'b JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsArray<'a, 'b> {
    #[varargs]
    #[constructor]
    fn constructor(&mut self, args: Vec<RuntimeValue<'a>>) {}

    #[varargs]
    #[callable]
    fn callable(thread: &mut JsThread<'a>, args: Vec<RuntimeValue<'a>>) {}

    #[named("reduceRight")]
    fn reduce_right() -> JsResult<'a> {
        todo!("Reduce right is not supported")
    }

    fn map(&mut self) -> JsResult<'a> {
        todo!("map is not supported")
    }

    fn pop(&mut self) -> RuntimeValue<'a> {
        if let Some(properties) = self.object.get_indexed_properties().as_mut() {
            properties.pop().unwrap_or_default()
        } else {
            RuntimeValue::Undefined
        }
    }

    fn isArray() -> bool {
        false
    }

    #[named("push")]
    #[varargs]
    fn push(&mut self, value: Vec<RuntimeValue<'a>>) {
        let maybe_indexed_properties = &mut self.object.get_indexed_properties();

        let indexed_properties =
            maybe_indexed_properties.get_or_insert(Vec::with_capacity(value.len()));

        indexed_properties.extend(value);
    }
}
