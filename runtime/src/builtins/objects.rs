use crate::result::JsResult;
use crate::{JsObject, JsThread, RuntimeValue};
use builtin::{named, prototype};

pub(crate) struct JsObjectBase<'a, 'b> {
    object: JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
impl<'a, 'b> JsObjectBase<'a, 'b> {
    #[named("getOwnPropertyDescriptor")]
    fn get_property_descriptor(&mut self) -> JsResult<'a> {
        let object = JsObject::new();
        object.set("enumerable".into(), false);
        object.set("writable".into(), true);
        Ok(object.into())
    }

    #[named("defineProperty")]
    fn define_property(
        &mut self,
        key: RuntimeValue<'a>,
        property: RuntimeValue<'a>,
    ) -> JsResult<'a> {
        // self.object.define_property()

        Ok(RuntimeValue::Undefined)
    }
}
