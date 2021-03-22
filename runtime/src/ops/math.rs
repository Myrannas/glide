use crate::{JsThread, RuntimeValue};

pub(crate) fn add(thread: &mut JsThread, times: u8) {
    let left: RuntimeValue = pop!(thread);

    match left {
        RuntimeValue::String(l_str) => {
            let l_str = thread.realm.strings.get(l_str).as_ref().to_owned();

            let mut string = l_str;
            for _ in 0..times {
                let right: RuntimeValue = pop!(thread);

                let r_str = catch!(thread, right.to_string(thread));
                let r_str = thread.realm.strings.get(r_str).as_ref();

                string += r_str;
            }

            let pointer = thread.realm.strings.intern(string);
            thread.push_stack(pointer);
        }
        other => {
            let l_val: f64 = other.to_number(&thread.realm);

            let mut value = l_val;
            for _ in 0..times {
                let right: RuntimeValue = pop!(thread);

                let r_val = right.to_number(&thread.realm);

                value += r_val;
            }

            thread.push_stack(value);
        }
    }

    thread.step();
}

pub(crate) fn numeric_op(thread: &mut JsThread, function: impl FnOnce(f64, f64) -> f64) {
    let l_prim: RuntimeValue = pop!(thread);
    let l_prim = l_prim.to_number(&thread.realm);

    let r_prim: RuntimeValue = pop!(thread);
    let r_prim = r_prim.to_number(&thread.realm);

    let result = function(l_prim, r_prim);

    thread.push_stack(result);

    thread.step();
}

pub(crate) fn subtract(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 - v2)
}

pub(crate) fn multiply(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 * v2)
}

pub(crate) fn divide(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 / v2)
}

pub(crate) fn modulus(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 % v2)
}

pub(crate) fn exponential(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1.powf(v2))
}

pub(crate) fn negate(thread: &mut JsThread) {
    let l_prim: RuntimeValue = pop!(thread);
    let l_prim = l_prim.to_number(&thread.realm);

    thread.push_stack(-l_prim);
    thread.step();
}

pub(crate) fn increment(thread: &mut JsThread, by: f64) {
    // Reference ->
    let target: RuntimeValue = thread.pop_stack();

    catch!(
        thread,
        target.update_reference(thread, |value, thread| {
            Ok(RuntimeValue::Float(value.to_number(&thread.realm) + by))
        })
    );

    thread.step();
}
