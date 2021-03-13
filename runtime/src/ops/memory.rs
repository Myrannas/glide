use crate::values::function::{CustomFunctionReference, FunctionReference, Prototype};
use crate::values::value::{InternalValue, Reference};
use crate::{InternalError, JsThread, RuntimeValue};
use instruction_set::{Constant, Environmental};

pub(crate) fn set(thread: &mut JsThread) {
    let value: RuntimeValue = pop!(thread);
    let attribute = pop!(thread);

    let target: RuntimeValue = pop!(thread, "Need a target");

    if target == RuntimeValue::Undefined {
        return thread.throw(
            thread
                .global_this
                .errors
                .new_type_error(format!("Cannot set property {} of undefined", attribute)),
        );
    }

    let target = target.to_object(thread);

    if let RuntimeValue::String(str) = attribute {
        target.set(str, value);
        // println!("{:?} {:?}", target, obj);
    } else if let RuntimeValue::Float(number) = attribute {
        target.set_indexed(number as usize, value);
    } else {
        thread.throw(InternalError::new_stackless(format!(
            "Cannot set attribute: {:?}",
            attribute
        )))
    };

    thread.step();
}

pub(crate) fn get(thread: &mut JsThread) {
    let attribute: RuntimeValue = pop!(thread);
    let target: RuntimeValue = pop!(thread);

    if target == RuntimeValue::Undefined {
        return thread.throw(
            thread
                .global_this
                .errors
                .new_type_error(format!("Cannot get property {} of undefined", attribute)),
        );
    }

    let target = target.to_object(thread);

    let reference = match attribute {
        RuntimeValue::Float(index) => RuntimeValue::Reference(Reference::Number {
            base: Box::new(target),
            name: index as usize,
            strict: true,
        }),
        value => RuntimeValue::Reference(Reference::String {
            base: Box::new(target),
            name: catch!(thread, value.to_string(thread)),
            strict: true,
        }),
    };

    thread.push_stack(reference);
    thread.step();
}

pub(crate) fn set_named(thread: &mut JsThread, atom: usize) {
    let value: RuntimeValue = pop!(thread);
    let target: RuntimeValue = pop!(thread, "Need a target");
    let target = target.to_object(thread);

    let atom = thread.current_function().get_atom(atom);

    // println!("Set named, target {:?}.{} = {:?}", target, atom, value);

    target.set(atom, value);

    thread.step();
}

pub(crate) fn set_local(thread: &mut JsThread, local: usize) {
    let value = pop!(thread);
    thread.current_context().write(local, value);
    thread.step();
}

pub(crate) fn get_local(thread: &mut JsThread, local: usize) {
    thread.push_stack(RuntimeValue::Internal(InternalValue::Local(local)));
    thread.step();
}

pub(crate) fn get_capture(thread: &mut JsThread, offset: usize, local: usize) {
    let value = thread.current_context().capture(offset - 1, local);
    thread.push_stack(value);
    thread.step();
}

pub(crate) fn set_capture(thread: &mut JsThread, offset: usize, local: usize) {
    let value = pop!(thread);
    thread
        .current_context()
        .write_capture(offset - 1, local, value);
    thread.step();
}

pub(crate) fn get_function(thread: &mut JsThread, function_id: usize) {
    let function = thread.current_function();
    let child_function = function.child_function(function_id).clone();

    let value = thread.global_this.wrappers.wrap_function(
        function.name(),
        FunctionReference::Custom(CustomFunctionReference {
            function: child_function,
            parent_context: thread.current_context().clone(),
        }),
    );

    thread.push_stack(value);
    thread.step();
}

pub(crate) fn get_class(thread: &mut JsThread, class_id: usize, extends: bool) {
    let function = thread.current_function();

    let child_class = function.child_class(class_id).clone();

    let prototype = if extends {
        let prototype: RuntimeValue = pop!(thread);

        match prototype {
            RuntimeValue::Object(obj) => Prototype::Custom(obj),
            RuntimeValue::Null => Prototype::Null,
            value => {
                return thread.throw(thread.global_this.errors.new_type_error(format!(
                    "Class extends value {} is not a constructor or null",
                    value,
                )))
            }
        }
    } else {
        Prototype::Object
    };

    let class = catch!(
        thread,
        child_class.load(thread.current_context(), &thread.global_this, prototype)
    );

    thread.push_stack(class);
    thread.step();
}

pub(crate) fn get_named(thread: &mut JsThread, atom: usize) {
    let atom = thread.current_function().get_atom(atom);
    let target: RuntimeValue = pop!(thread, "Need a target");

    if target == RuntimeValue::Undefined {
        return thread.throw(
            thread
                .global_this
                .errors
                .new_type_error(format!("Cannot get property {} of undefined", atom)),
        );
    }

    let target = target.to_object(thread);

    // println!("Get named, target {:?}.{}", target, atom);

    thread.push_stack(RuntimeValue::Reference(Reference::String {
        base: Box::new(target),
        name: atom,
        strict: true,
    }));

    thread.step();
}

pub(crate) fn load_constant(thread: &mut JsThread, constant: &Constant) {
    thread.stack.push(constant.into());

    thread.step();
}

pub(crate) fn load_environmental(thread: &mut JsThread, environmental: &Environmental) {
    let value = match environmental {
        Environmental::GlobalThis => thread.global_this.global_this.clone(),
        Environmental::This => thread.current_context().this().clone(),
        Environmental::NewObject => thread.global_this.wrappers.new_object(),
    };

    thread.push_stack(value);
    thread.step();
}

pub(crate) fn delete(thread: &mut JsThread) {
    todo!("Not yet working");
}

pub(crate) fn resolve(thread: &mut JsThread) {
    let value: RuntimeValue = pop!(thread);

    thread.push_stack(value);
    thread.step();
}

pub(crate) fn duplicate(thread: &mut JsThread) {
    let element = thread.stack.last().unwrap_or_default().clone();

    thread.stack.push(element);
    thread.step();
}
