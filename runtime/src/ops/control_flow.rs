use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::values::object::FunctionObject;
use crate::values::value::Reference;
use crate::{ExecutionError, InternalError, JsObject, JsThread, RuntimeValue};
use instruction_set::Constant;

fn get_callable<'a>(
    thread: &mut JsThread<'a>,
    value: RuntimeValue<'a>,
) -> JsResult<'a, impl FunctionObject<'a>> {
    // 1. Assert: F is an ECMAScript function object.
    let fn_object = match &value {
        RuntimeValue::Object(obj) => obj.function(),
        _ => None,
    };

    if let Some(fn_object) = fn_object {
        Ok(fn_object)
    } else {
        Err(ExecutionError::Thrown(
            thread
                .global_this
                .errors
                .new_type_error(format!("{} is not a function", value)),
            None,
        ))
    }
}

/*
    https://262.ecma-international.org/11.0/#sec-ecmascript-function-objects-call-thisargument-argumentslist

    [[Call]] ( thisArgument, argumentsList )

    The [[Call]] internal method for an ECMAScript function object F is called with parameters thisArgument and argumentsList, a List of ECMAScript language values.
*/
pub(crate) fn call(thread: &mut JsThread, args: usize) {
    let fn_value: RuntimeValue = thread.pop_stack();

    let target = match fn_value.clone() {
        RuntimeValue::Reference(Reference::String { base, .. }) => *base,
        RuntimeValue::Reference(Reference::Number { base, .. }) => *base,
        other => resolve!(other, thread).to_object(thread),
    };

    let fn_object = resolve!(fn_value.clone(), thread);
    let fn_object = catch!(thread, get_callable(thread, fn_object));

    // 2. If F.[[IsClassConstructor]] is true, throw a TypeError exception.
    if fn_object.is_class_constructor() {
        let message = if let Some(name) = fn_object.name().as_ref() {
            format!("Class constructor {} cannot be invoked without 'new'", name)
        } else {
            "Class constructors cannot be invoked without 'new'".to_owned()
        };

        return thread.throw(thread.global_this.errors.new_type_error(message));
    }

    let callable = fn_object.callable();
    match callable.as_ref().unwrap() {
        FunctionReference::Custom(function) => {
            thread.call(target, function.clone(), args, false, false);
        }
        FunctionReference::BuiltIn(function) => {
            function.apply(args, thread, Some(target));
            thread.step();
        }
    }
}

/*
    https://262.ecma-international.org/11.0/#sec-ecmascript-function-objects-construct-argumentslist-newtarget

    9.2.2 [[Construct]] ( argumentsList, newTarget )

    The [[Construct]] internal method for an ECMAScript function object F is called with parameters argumentsList and newTarget. argumentsList is a possibly empty List of ECMAScript language values.
*/
pub(crate) fn call_new(thread: &mut JsThread, args: usize) {
    let fn_value: RuntimeValue = thread.pop_stack();

    let resolved_value = resolve!(fn_value.clone(), thread);
    let fn_object = catch!(thread, get_callable(thread, resolved_value.clone()));

    let callable = if let Some(callable) = fn_object.construct().as_ref() {
        callable.clone()
    } else {
        // `object.is_callable()` asserts that there must be a callable
        fn_object.callable().as_ref().unwrap().clone()
    };

    let target = JsObject::new();

    if let Some(name) = fn_object.name().as_ref() {
        target.set_name(name.clone());
    }

    if let Some(prototype) = fn_object.prototype().as_ref() {
        target.set_prototype(prototype.clone());
    }

    target.set("constructor".into(), resolved_value);

    match callable {
        FunctionReference::Custom(function) => {
            thread.call(target, function.clone(), args, true, false);
        }
        FunctionReference::BuiltIn(function) => {
            catch!(
                thread,
                function.apply_return(args, thread, Some(target.clone()))
            );

            thread.push_stack(target);
            thread.step();
        }
    };
}

pub(crate) fn jump(thread: &mut JsThread, to: usize) {
    thread.jump(to)
}

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

pub(crate) fn return_value(thread: &mut JsThread) {
    if thread.is_new() {
        let this = thread.current_context().this().clone();

        thread.return_value(this);
    } else {
        let return_value: RuntimeValue = thread.pop_stack();

        thread.return_value(return_value)
    };
}

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
