use crate::result::JsResult;
use crate::values::nan::Value;
use crate::{BuiltIn, JsObject, JsThread};
use builtin::{constructor, named, prototype};

pub(crate) struct JsPromise<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Promise")]
impl<'a, 'b> JsPromise<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, _resolver: Option<Value<'a>>) -> JsResult<'a, ()> {
        let resolve = JsObject::builder(&mut self.thread.realm.objects)
            .with_callable(BuiltIn {
                context: None,
                op: |_, _, _, _| Ok(None),
                name: None,
            })
            .build();

        let reject = JsObject::builder(&mut self.thread.realm.objects)
            .with_callable(BuiltIn {
                context: None,
                op: |_, _, _, _| Ok(None),
                name: None,
            })
            .build();

        self.target
            .to_object(&mut self.thread.realm)?
            .call(self.thread, &[resolve.into(), reject.into()])?;

        Ok(())
    }

    #[allow(clippy::unused_self)]
    fn then(&mut self) {}

    #[allow(clippy::unused_self)]
    fn catch(&mut self) {}

    #[allow(clippy::unused_self)]
    fn finally(&mut self) {}

    fn all(_: &mut JsThread<'a>) {}

    #[named("allSettled")]
    fn all_settled(_: &mut JsThread<'a>) {}

    fn any(_: &mut JsThread<'a>) {}

    fn race(_: &mut JsThread<'a>) {}

    fn resolve(_: &mut JsThread<'a>) {}

    fn reject(_: &mut JsThread<'a>) {}
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
