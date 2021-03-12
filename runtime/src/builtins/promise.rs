use crate::builtins::objects::JsObjectBase;
use crate::result::JsResult;
use crate::values::function::CustomFunctionReference;
use crate::values::object::FunctionObject;
use crate::{BuiltIn, JsObject, JsThread, RuntimeValue};
use builtin::{constructor, named, prototype};

pub(crate) struct JsPromise<'a, 'b> {
    object: JsObject<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Promise")]
impl<'a, 'b> JsPromise<'a, 'b> {
    #[constructor]
    fn constructor(&mut self, resolver: Option<RuntimeValue<'a>>) -> JsResult<'a, ()> {
        let resolver_function = resolver.unwrap_or_default();

        let resolver_function = match resolver_function {
            RuntimeValue::Object(obj) if obj.is_function() => obj.function().unwrap(),
            other => {
                return Err(self
                    .thread
                    .global_this
                    .errors
                    .new_type_error(format!("Promise resolver {} is not a function", other))
                    .into())
            }
        };

        self.thread.push_stack(
            JsObject::builder()
                .with_callable(BuiltIn {
                    context: None,
                    op: |_, _, _, _| Ok(None),
                })
                .build(),
        );

        self.thread.push_stack(
            JsObject::builder()
                .with_callable(BuiltIn {
                    context: None,
                    op: |_, _, _, _| Ok(None),
                })
                .build(),
        );

        self.thread.call_from_native(
            self.thread.global_this.global_this.clone(),
            resolver_function.callable().clone().unwrap(),
            2,
            false,
        )?;

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
