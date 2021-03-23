use crate::debugging::DebuggableWithThread;
use crate::object_pool::ObjectPointer;
use crate::primordials::RuntimeHelpers;
use crate::result::JsResult;
use crate::values::function::FunctionReference;
use crate::{ExecutionError, JsObject, JsThread, Value, ValueType};
use instruction_set::Constant;

fn get_callable<'a>(
    thread: &mut JsThread<'a>,
    value: Value<'a>,
) -> JsResult<'a, ObjectPointer<'a>> {
    // 1. Assert: F is an ECMAScript function object.

    match value.get_type() {
        ValueType::Object(obj) if obj.is_callable(&thread.realm.objects) => Ok(obj),
        _ => Err(ExecutionError::Thrown(
            thread
                .new_type_error(format!(
                    "{:?} is not a function",
                    DebuggableWithThread::from(&value, thread)
                ))
                .into(),
            None,
        )),
    }
}

/*
    https://262.ecma-international.org/11.0/#sec-ecmascript-function-objects-call-thisargument-argumentslist

    [[Call]] ( thisArgument, argumentsList )

    The [[Call]] internal method for an ECMAScript function object F is called with parameters thisArgument and argumentsList, a List of ECMAScript language values.
*/
pub(crate) fn call(thread: &mut JsThread, args: usize) {
    let fn_value: Value = thread.pop_stack();

    // ??? -> This is a bit tricky to evaluate.
    let target = match fn_value.get_type() {
        ValueType::StringReference(_) | ValueType::NumberReference(_) => {
            let last = *(thread.stack.last().unwrap());

            catch!(thread, last.to_object(thread))
        }
        _ => catch!(thread, resolve!(fn_value, thread).to_object(thread)),
    };

    let fn_object = resolve!(fn_value, thread);
    let fn_object = catch!(thread, get_callable(thread, fn_object));

    // 2. If F.[[IsClassConstructor]] is true, throw a TypeError exception.
    if fn_object.is_class_constructor(&thread.realm.objects) {
        let message = if let Some(name) = fn_object.get_name(&thread.realm.objects) {
            format!("Class constructor {} cannot be invoked without 'new'", name)
        } else {
            "Class constructors cannot be invoked without 'new'".to_owned()
        };

        let error = thread.new_type_error(message);
        return thread.throw(error);
    }

    let callable = fn_object
        .get_callable(&thread.realm.objects)
        .unwrap()
        .clone();
    match callable {
        FunctionReference::Custom(function) => {
            thread.call(target.into(), function.clone(), args, false, false);
        }
        FunctionReference::BuiltIn(function) => {
            let result = catch!(thread, function.apply_return(args, thread, target.into()));

            thread.push_stack(result.unwrap_or_default());

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
    let fn_value: Value = thread.pop_stack();

    let resolved_value = resolve!(fn_value, thread);
    let fn_object = catch!(thread, get_callable(thread, resolved_value));

    let callable = if let Some(constructor) = fn_object.get_construct(&thread.realm.objects) {
        constructor.clone()
    } else {
        let error = thread.new_type_error("is not a constructor");
        return thread.throw(ExecutionError::Thrown(error.into(), None));
    };

    let mut target = JsObject::new();

    if let Some(name) = fn_object.get_name(&thread.realm.objects) {
        target.set_name(name);
    }

    if let Some(prototype) = fn_object.get_prototype(thread) {
        target.set_prototype(prototype);
    }

    target.set(thread.realm.constants.constructor, resolved_value);

    let target_pointer = thread.realm.objects.allocate(target);

    match callable {
        FunctionReference::Custom(function) => {
            thread.call(target_pointer.into(), function, args, true, false);
        }
        FunctionReference::BuiltIn(function) => {
            catch!(
                thread,
                function.apply_return(args, thread, target_pointer.into())
            );

            thread.push_stack(target_pointer);
            thread.step();
        }
    };
}

pub(crate) fn jump(thread: &mut JsThread, to: usize) {
    thread.jump(to)
}

pub(crate) fn compare_jump(thread: &mut JsThread, if_true: usize, if_false: usize) {
    if pop!(thread).to_bool(&thread.realm) {
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
        let this = thread.current_context().this();

        thread.return_value(this);
    } else {
        let return_value = pop!(thread);

        thread.return_value(return_value)
    };
}

pub(crate) fn return_constant(thread: &mut JsThread, constant: &Constant) {
    if thread.is_new() {
        let this = thread.current_context().this();

        thread.return_value(this);
    } else {
        let c = Value::from_constant(thread.current_function().atoms(), *constant);

        thread.return_value(c);
    };
}

pub(crate) fn throw_value(thread: &mut JsThread) {
    let value = pop!(thread);

    thread.throw(value)
}
