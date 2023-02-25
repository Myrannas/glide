use crate::{catch, pop, JsThread, Value, ValueType};

pub(crate) fn add(thread: &mut JsThread, times: u8) {
    let left: Value = pop!(thread);

    if let ValueType::String(l_str) = left.get_type() {
        let l_str = thread.realm.strings.get(l_str).as_ref().to_owned();

        let mut string = l_str;
        for _ in 0..times {
            let right: Value = pop!(thread);

            let r_str = catch!(thread, right.to_string(thread));
            let r_str = thread.realm.strings.get(r_str).as_ref();

            string += r_str;
        }

        let pointer = thread.realm.strings.intern(string);
        thread.push_stack(pointer);
    } else {
        let l_val: f64 = left.to_number(&thread.realm);

        let mut value = l_val;
        for _ in 0..times {
            let right: Value = pop!(thread);

            let r_val = right.to_number(&thread.realm);

            value += r_val;
        }

        thread.push_stack(value);
    }

    thread.step();
}

pub(crate) fn numeric_op(thread: &mut JsThread, function: impl FnOnce(f64, f64) -> f64) {
    let l_prim: Value = pop!(thread);
    let l_prim = l_prim.to_number(&thread.realm);

    let r_prim: Value = pop!(thread);
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
    let l_prim: Value = pop!(thread);
    let l_prim = l_prim.to_number(&thread.realm);

    thread.push_stack(-l_prim);
    thread.step();
}

pub(crate) fn increment(thread: &mut JsThread, by: f64) {
    // Reference ->
    let target: Value = thread.pop_stack();

    catch!(
        thread,
        target.update_reference(thread, |value, thread| {
            let as_number = value.to_number(&thread.realm);
            thread.push_stack(as_number);
            Ok(Value::from(as_number + by))
        })
    );

    thread.step();
}
