use crate::primordials::RuntimeHelpers;
use crate::values::function::{CustomFunctionReference, FunctionReference, Prototype};
use crate::values::nan::Value;

use crate::debugging::DebugWithRealm;
use crate::{catch, pop, InternalError, JsThread, ValueType};
use instruction_set::{Constant, Environmental, PrivateValue};

pub(crate) fn set(thread: &mut JsThread) {
    let value = pop!(thread);
    let attribute = pop!(thread);

    let target = pop!(thread, "Need a target");

    if target == Value::UNDEFINED {
        let str_value = catch!(thread, attribute.to_string(thread));

        let type_error = thread.new_type_error(format!(
            "Cannot set property {:?} of undefined",
            thread.realm.get_string(str_value)
        ));

        return thread.throw(type_error);
    }

    let target = catch!(thread, target.to_object(&mut thread.realm));

    match attribute.get_type() {
        ValueType::String(str) => target.set(&mut thread.realm.objects, str, value),
        ValueType::Float => target.set_indexed(thread, attribute.float() as usize, value),
        _ => thread.throw(InternalError::new_stackless(format!(
            "Cannot set attribute: {}",
            thread.debug_value(&attribute)
        ))),
    }

    thread.step();
}

pub(crate) fn get(thread: &mut JsThread) {
    let attribute = pop!(thread);
    let target = pop!(thread);

    if target == Value::UNDEFINED {
        let attribute = catch!(thread, attribute.to_string(thread));
        let attribute = thread.realm.strings.get(attribute);

        let error_message = format!("Cannot get property {} of undefined", attribute);

        let type_error = thread.new_type_error(error_message);

        return thread.throw(type_error);
    }

    let target = catch!(thread, target.to_object(&mut thread.realm));

    let reference: Value = match attribute.get_type() {
        ValueType::Float => ValueType::NumberReference(attribute.float() as u32),
        _ => ValueType::StringReference(catch!(thread, attribute.to_string(thread))),
    }
    .into();

    thread.push_stack(target);
    thread.push_stack(reference);
    thread.step();
}

pub(crate) fn set_named(thread: &mut JsThread, atom: usize) {
    let value = pop!(thread);
    let target = pop!(thread, "Need a target");

    // println!("Get named, target {:?}.{}", target, atom);

    let target = catch!(thread, target.to_object(&mut thread.realm));

    let atom = thread.current_function().get_atom(atom);

    // println!("Set named, target {:?}.{} = {:?}", target, atom, value);

    target.set(&mut thread.realm.objects, atom, value);

    thread.step();
}

pub(crate) fn set_local(thread: &mut JsThread, local: usize) {
    let value = pop!(thread);
    thread.current_context().write(local, value);
    thread.step();
}

pub(crate) fn get_local(thread: &mut JsThread, local: usize) {
    thread.push_stack(ValueType::Local(local as u32));
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
    let name = function.name();
    let child_function = function.child_function(function_id).clone();

    let function_reference = FunctionReference::Custom(CustomFunctionReference {
        function: child_function,
        parent_context: thread.current_context().clone(),
    });

    let value = thread.new_function(name, function_reference);

    // println!("{:?}", value);

    thread.push_stack(value);
    thread.step();
}

pub(crate) fn get_class(thread: &mut JsThread, class_id: usize, extends: bool) {
    let function = thread.current_function();

    let child_class = function.child_class(class_id).clone();

    let prototype = if extends {
        let prototype = pop!(thread);

        match prototype.get_type() {
            ValueType::Object(obj) => Prototype::Custom(obj),
            ValueType::Null => Prototype::Null,
            _ => {
                let type_error = thread.new_type_error(format!(
                    "Class extends value {} is not a constructor or null",
                    thread.debug_value(&prototype),
                ));

                return thread.throw(type_error);
            }
        }
    } else {
        Prototype::Object
    };

    let class = catch!(
        thread,
        child_class.load(&thread.current_context().clone(), thread, prototype)
    );

    thread.push_stack(class);
    thread.step();
}

pub(crate) fn get_named(thread: &mut JsThread, atom: usize) {
    let atom = thread.current_function().get_atom(atom);
    let target = pop!(thread, "Need a target");

    if target == Value::UNDEFINED {
        let attribute = thread.realm.strings.get(atom);
        let message = format!("Cannot get property {} of undefined", attribute);

        let type_error = thread.new_type_error(message);

        return thread.throw(type_error);
    }

    let target = catch!(thread, target.to_object(&mut thread.realm));

    thread.push_stack(target);
    thread.push_stack(ValueType::StringReference(atom));

    thread.step();
}

pub(crate) fn get_private(thread: &mut JsThread, PrivateValue(index): &PrivateValue) {
    let this = thread.current_context().this();

    let value: Value = if let ValueType::Object(ptr) = this.get_type() {
        thread
            .realm
            .get_object(ptr)
            .private_properties
            .get(*index)
            .cloned()
            .unwrap_or(Value::UNDEFINED)
    } else {
        Value::UNDEFINED
    };

    thread.push_stack(value);
    thread.step();
}

pub(crate) fn set_private(thread: &mut JsThread, PrivateValue(index): &PrivateValue) {
    let this = thread.current_context().this();
    let top = pop!(thread);

    if let ValueType::Object(ptr) = this.get_type() {
        thread.realm.get_object_mut(ptr).private_properties[*index] = top;
    };

    thread.step();
}

pub(crate) fn load_constant(thread: &mut JsThread, constant: &Constant) {
    thread.push_stack(Value::from_constant(
        thread.current_function().atoms(),
        *constant,
    ));

    thread.step();
}

pub(crate) fn load_environmental(thread: &mut JsThread, environmental: &Environmental) {
    let value: Value = match environmental {
        Environmental::GlobalThis => thread.realm.global_this.into(),
        Environmental::This => thread.current_context().this(),
        Environmental::NewObject => thread
            .realm
            .wrappers
            .new_object(&mut thread.realm.objects)
            .into(),
    };
    //
    // println!(
    //     "Load environment: {:?}",
    //     DebuggableWithThread::from(&value, thread)
    // );

    thread.push_stack(value);
    thread.step();
}

pub(crate) fn delete(thread: &mut JsThread) {
    let target: Value = thread.pop_stack();

    catch!(thread, target.delete_reference(thread));

    thread.push_stack(Value::TRUE);

    thread.step();
}

pub(crate) fn resolve(thread: &mut JsThread) {
    let value = pop!(thread);

    thread.push_stack(value);
    thread.step();
}

pub(crate) fn duplicate(thread: &mut JsThread) {
    let element = thread.stack.last().cloned().unwrap_or_default();

    thread.stack.push(element);
    thread.step();
}
