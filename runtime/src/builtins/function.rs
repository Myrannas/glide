use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::function::{CustomFunctionReference, FunctionReference};
use crate::values::nan::{Value, ValueType};
use crate::{ExecutionError, InternalError, JsThread};
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
        let args_with_callable = args.len();
        let args_len = if args_with_callable > 0 {
            args_with_callable - 1
        } else {
            0
        };

        for arg in args.into_iter().skip(1) {
            self.thread.push_stack(arg);
        }

        let callable = self
            .target
            .to_object(&mut self.thread.realm)?
            .get_callable(&self.thread.realm.objects)
            .cloned();

        if let Some(callable) = callable {
            let result = self
                .thread
                .call_from_native(target, callable, args_len, false)?;

            return Ok(result);
        }

        Err(self
            .thread
            .new_type_error("Can't use Function.call on a non-function")
            .into())
    }

    fn apply(&mut self, target: Value<'a>, values: Value<'a>) -> JsResult<'a> {
        let args = if let ValueType::Object(value) = values.get_type() {
            value
        } else {
            return Err(self.thread.new_type_error("Cant apply non-array").into());
        };

        let args = args.get_object(&self.thread.realm.objects);

        for arg in args.indexed_properties.iter().skip(1) {
            self.thread.stack.push(*arg);
        }

        let args_len = args.indexed_properties.len();

        let callable = self
            .target
            .to_object(&mut self.thread.realm)?
            .get_callable(&self.thread.realm.objects)
            .cloned();

        if let Some(callable) = callable {
            let result = self
                .thread
                .call_from_native(target, callable, args_len, false)?;

            return Ok(result);
        }

        InternalError::new_stackless("Can't use Function.call on a non-function").into()
    }

    #[named("toString")]
    fn to_string(&mut self) -> JsResult<'a> {
        if let Some(function) = self
            .target
            .to_object(&mut self.thread.realm)?
            .get_callable(&self.thread.realm.objects)
        {
            let name = match function {
                FunctionReference::BuiltIn(builtin) => builtin.name,
                FunctionReference::Custom(CustomFunctionReference { function, .. }) => {
                    Some(function.name())
                }
            };

            let name =
                name.unwrap_or_else(|| self.thread.realm.strings.intern_native("(anonymous)"));

            Ok(self
                .thread
                .realm
                .strings
                .intern(format!("[Function: {}]", name))
                .into())
        } else {
            Err(ExecutionError::TypeError(
                "Function.prototype.toString requires that 'this' be a Function".to_string(),
            ))
        }
    }
}
