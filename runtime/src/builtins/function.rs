use crate::object_pool::ObjectPointer;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::values::nan::{Value, ValueType};
use crate::{ExecutionError, InternalError, JsObject, JsThread};
use builtin::{named, prototype, varargs};

pub(crate) struct JsFunctionObject<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Function")]
impl<'a, 'b> JsFunctionObject<'a, 'b> {
    #[varargs]
    fn call(&mut self, args: Vec<Value<'a>>) -> JsResult<'a> {
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
            let result = self
                .thread
                .call_from_native(target.into(), callable, args_len, false)?;

            return Ok(result.into());
        }

        InternalError::new_stackless("Can't use Function.call on a non-function").into()
    }

    fn apply(&mut self, target: Value<'a>, values: &Value<'a>) -> JsResult<'a> {
        let args = if let ValueType::Object(value) = values.get_type() {
            value
        } else {
            return Err(self.thread.new_type_error("Cant apply non-array").into());
        };

        let args = args.get_object(&self.thread.realm.objects);

        for arg in args.indexed_properties.iter().skip(1) {
            self.thread.stack.push(arg.clone().into());
        }

        let args_len = args.indexed_properties.len();

        let callable = self
            .target
            .to_object(self.thread)?
            .get_callable(&self.thread.realm.objects)
            .cloned();

        if let Some(callable) = callable {
            let result = self
                .thread
                .call_from_native(target.into(), callable, args_len, false)?;

            return Ok(result.into());
        }

        InternalError::new_stackless("Can't use Function.call on a non-function").into()
    }
}
