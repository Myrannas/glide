use crate::{JsThread, RuntimeValue};

#[inline]
pub(crate) fn add(thread: &mut JsThread) {
    let left: RuntimeValue = pop!(thread);
    let right: RuntimeValue = pop!(thread);

    match left {
        RuntimeValue::String(l_str) => {
            let r_str = catch!(thread, right.to_string(thread));
            let string = l_str.as_ref().to_owned() + r_str.as_ref();

            thread.push_stack(string);
        }
        other => {
            let l_val: f64 = other.into();
            let r_val: f64 = right.into();

            thread.push_stack(r_val + l_val);
        }
    }

    thread.step();
}

#[inline]
pub(crate) fn numeric_op(thread: &mut JsThread, function: impl FnOnce(f64, f64) -> f64) {
    let l_prim: f64 = pop!(thread);
    let r_prim: f64 = pop!(thread);
    let result = function(l_prim, r_prim);

    thread.push_stack(result);

    thread.step();
}

#[inline]
pub(crate) fn subtract(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 - v2)
}

#[inline]
pub(crate) fn multiply(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 * v2)
}

#[inline]
pub(crate) fn divide(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 / v2)
}

#[inline]
pub(crate) fn modulus(thread: &mut JsThread) {
    numeric_op(thread, |v1, v2| v1 % v2)
}

#[inline]
pub(crate) fn negate(thread: &mut JsThread) {
    let l_prim: f64 = pop!(thread);

    thread.push_stack(-l_prim);
    thread.step();
}

pub(crate) fn increment(thread: &mut JsThread, by: f64) {
    let target: RuntimeValue = thread.pop_stack();

    let value = resolve!(target.clone(), thread);

    thread.stack.push(value.clone());

    let value: f64 = value.into();

    catch!(thread, target.update_reference(thread, value + by));

    thread.step();
}
