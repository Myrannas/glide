use crate::object_pool::ObjectPointer;
use crate::result::JsResult;
use crate::{JsObject, JsThread, RuntimeValue};
use builtin::{constructor, getter, named, prototype, varargs};

pub(crate) struct JsArray<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsArray<'a, 'b> {
    #[varargs]
    #[constructor]
    fn constructor(&mut self, args: Vec<RuntimeValue<'a>>) {
        if args.len() > 1 {
            self.object
                .get_mut_object(self.thread)
                .set_indexed_properties(args)
        }
    }

    #[named("reduceRight")]
    fn reduce_right() -> JsResult<'a> {
        todo!("Reduce right is not supported")
    }

    fn map(&mut self) -> JsResult<'a> {
        todo!("map is not supported")
    }

    fn pop(&mut self) -> RuntimeValue<'a> {
        self.object
            .get_mut_object(self.thread)
            .get_mut_indexed_properties()
            .pop()
            .unwrap_or_default()
    }

    #[named("isArray")]
    fn is_array() -> bool {
        false
    }

    #[named("push")]
    #[varargs]
    fn push(&mut self, value: Vec<RuntimeValue<'a>>) {
        self.object
            .get_mut_object(self.thread)
            .get_mut_indexed_properties()
            .extend(value);
    }

    #[getter]
    fn length(&mut self) -> f64 {
        self.object
            .get_object(self.thread)
            .get_indexed_properties()
            .len() as f64
    }
}
