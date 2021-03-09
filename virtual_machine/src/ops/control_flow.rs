use crate::function::FunctionReference;
use crate::value::Reference;
use crate::{InternalError, JsObject, JsThread, RuntimeValue};
use instruction_set::Constant;

#[inline]
pub(crate) fn call(thread: &mut JsThread, args: usize) {
    let fn_value: RuntimeValue = thread.pop_stack();

    let target = match fn_value.clone() {
        RuntimeValue::Reference(Reference { base, .. }) => *base,
        other => resolve!(other, thread).to_object(thread),
    };

    match resolve!(fn_value.clone(), thread) {
        RuntimeValue::Object(obj) if obj.is_callable() => match obj.get_callable().unwrap() {
            FunctionReference::Custom(function) => {
                thread.call(target, function, args, false, false);
            }
            FunctionReference::BuiltIn(function) => {
                function.apply(args, thread, Some(target));
                thread.step();
            }
        },
        _ => thread.throw(
            thread
                .global_this
                .errors
                .new_type_error(format!("{} is not a function", fn_value)),
        ),
    };
}

#[inline]
pub(crate) fn call_new(thread: &mut JsThread, args: usize) {
    let fn_value: RuntimeValue = thread.pop_stack();

    match resolve!(fn_value, thread) {
        RuntimeValue::Object(obj) if obj.is_callable() => match obj.get_callable().unwrap() {
            FunctionReference::Custom(function) => {
                let mut target = JsObject::new().with_name(function.function.name());

                if let Some(prototype) = obj.prototype() {
                    target = target.with_prototype(prototype);
                }

                thread.call(target, function, args, true, false);
            }
            FunctionReference::BuiltIn(function) => {
                let mut target = JsObject::new();

                if let Some(prototype) = obj.prototype() {
                    target = target.with_prototype(prototype);
                }

                function.apply(args, thread, Some(target.clone()));

                thread.push_stack(target);
                thread.step();
            }
        },
        v => {
            return thread.throw(InternalError::new_stackless(format!(
                "Uncaught type error: {:?} is not a function",
                v
            )));
        }
    }
}

#[inline]
pub(crate) fn jump(thread: &mut JsThread, to: usize) {
    thread.jump(to)
}

#[inline]
pub(crate) fn compare_jump(thread: &mut JsThread, if_true: usize, if_false: usize) {
    if pop!(thread) {
        thread.jump(if_true);
    } else {
        thread.jump(if_false);
    }
}

pub(crate) fn catch(thread: &mut JsThread, chunk: usize) {
    thread.catch(chunk);
    thread.step();
}

pub(crate) fn drop_catch(thread: &mut JsThread, chunk: usize) {
    thread.drop_catch(chunk);
    thread.step();
}

#[inline]
pub(crate) fn return_value(thread: &mut JsThread) {
    if thread.is_new() {
        let this = thread.current_context().this().clone();

        thread.return_value(this);
    } else {
        let return_value: RuntimeValue = thread.pop_stack();

        thread.return_value(return_value)
    };
}

#[inline]
pub(crate) fn return_constant(thread: &mut JsThread, constant: &Constant) {
    if thread.is_new() {
        let this = thread.current_context().this().clone();

        thread.return_value(this);
    } else {
        thread.return_value(constant)
    };
}

pub(crate) fn throw_value(thread: &mut JsThread) {
    let value: RuntimeValue = pop!(thread);

    thread.throw(value)
}
