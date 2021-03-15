use crate::object_pool::ObjectPointer;
use crate::result::JsResult;
use crate::values::object::FunctionObject;
use crate::{BuiltIn, JsObject, JsThread, RuntimeValue};
use builtin::{constructor, named, prototype};

pub(crate) struct JsPromise<'a, 'b> {
    object: ObjectPointer<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Promise")]
impl<'a, 'b> JsPromise<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, resolver: Option<RuntimeValue<'a>>) -> JsResult<'a, ()> {
        let resolve = JsObject::builder()
            .with_callable(BuiltIn {
                context: None,
                op: |_, _, _, _| Ok(None),
            })
            .build(self.thread);

        let reject = JsObject::builder()
            .with_callable(BuiltIn {
                context: None,
                op: |_, _, _, _| Ok(None),
            })
            .build(self.thread);

        self.object
            .call(self.thread, &[resolve.into(), reject.into()])?;

        Ok(())
    }
    // #[named("getOwnPropertyDescriptor")]
    // fn get_property_descriptor(&mut self) -> JsResult<'a> {
    //     let object = JsObject::new();
    //     object.set("enumerable".into(), false);
    //     object.set("writable".into(), true);
    //     Ok(object.into())
    // }
    //
    // #[named("defineProperty")]
    // fn define_property(
    //     &mut self,
    //     key: RuntimeValue<'a>,
    //     property: RuntimeValue<'a>,
    // ) -> JsResult<'a> {
    //     // self.object.define_property()
    //
    //     Ok(RuntimeValue::Undefined)
    // }
}
