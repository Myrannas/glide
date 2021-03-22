use crate::object_pool::ObjectPointer;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::{ExecutionError, InternalError, JsObject, JsThread, RuntimeValue};
use builtin::{named, prototype, varargs};

pub(crate) struct JsFunctionObject<'a, 'b> {
    target: RuntimeValue<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Function")]
impl<'a, 'b> JsFunctionObject<'a, 'b> {
    #[varargs]
    fn call(&mut self, args: Vec<RuntimeValue<'a>>) -> JsResult<'a> {
        let target = args.first().cloned().unwrap_or_default();
        let args_len = args.len() - 1;

        for arg in args.into_iter().skip(1) {
            self.thread.push_stack(arg.clone());
        }

        let callable = self
            .target
            .to_object(self.thread)?
            .get_callable(&self.thread.realm.objects)
            .cloned();

        if let Some(callable) = callable {
            return self
                .thread
                .call_from_native(target, callable, args_len, false);
        }

        InternalError::new_stackless("Can't use Function.call on a non-function").into()
    }

    fn apply(&mut self, target: RuntimeValue<'a>, values: &RuntimeValue<'a>) -> JsResult<'a> {
        let args = if let RuntimeValue::Object(value) = values {
            value
        } else {
            return Err(self.thread.new_type_error("Cant apply non-array").into());
        };

        let args = args.get_object(&self.thread.realm.objects);

        for arg in args.indexed_properties.iter().skip(1) {
            self.thread.stack.push(arg.clone());
        }

        let args_len = args.indexed_properties.len();

        let callable = self
            .target
            .to_object(self.thread)?
            .get_callable(&self.thread.realm.objects)
            .cloned();

        if let Some(callable) = callable {
            return self
                .thread
                .call_from_native(target, callable, args_len, false);
        }

        InternalError::new_stackless("Can't use Function.call on a non-function").into()
    }
}
