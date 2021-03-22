use crate::primordials::RuntimeHelpers;
use crate::{JsThread, RuntimeValue};

pub(crate) fn equality<'a>(
    thread: &mut JsThread<'a>,
    operator: impl FnOnce(&mut JsThread<'a>, RuntimeValue<'a>, RuntimeValue<'a>) -> bool,
) {
    let l_ref = pop!(thread);
    let r_ref = pop!(thread);

    let result = operator(thread, l_ref, r_ref);

    thread.push_stack(result);
    thread.step();
}

pub(crate) fn strict_equal_to(thread: &mut JsThread) {
    equality(thread, |_, left, right| left.strict_eq(&right))
}

pub(crate) fn equal_to(thread: &mut JsThread) {
    equality(thread, |thread, left, right| {
        left.non_strict_eq(&right, thread)
    })
}

pub(crate) fn not_strict_equal_to(thread: &mut JsThread) {
    equality(thread, |_, left, right| !left.strict_eq(&right))
}

pub(crate) fn not_equal_to(thread: &mut JsThread) {
    equality(thread, |thread, left, right| {
        !left.non_strict_eq(&right, thread)
    })
}

pub(crate) fn instance_of(thread: &mut JsThread) {
    let left: RuntimeValue = pop!(thread);
    let right: RuntimeValue = pop!(thread);

    let result = if let (RuntimeValue::Object(left), RuntimeValue::Object(right)) = (left, right) {
        if let Some(right_proto) = right.get_prototype(thread) {
            let mut left_proto = left.get_prototype(thread);
            let mut result = false;
            while let Some(proto) = left_proto {
                if proto == right_proto {
                    result = true;
                    break;
                }

                left_proto = proto.get_prototype(thread);
            }

            result
        } else {
            false
        }
    } else {
        false
    };

    thread.push_stack(result);
    thread.step();
}

pub(crate) fn logical_or(thread: &mut JsThread, left: usize, right: usize) {
    let l_ref: RuntimeValue = thread.pop_stack();

    let l_prim = l_ref.clone();
    let l_prim = resolve!(l_prim, thread);

    if l_prim.to_bool(&thread.realm) {
        thread.push_stack(l_ref);

        thread.jump(left);
    } else {
        thread.jump(right);
    }
}

pub(crate) fn logical_and(thread: &mut JsThread, left: usize, right: usize) {
    let l_ref: RuntimeValue = pop!(thread);

    if l_ref.to_bool(&thread.realm) {
        thread.jump(right);
    } else {
        thread.push_stack(l_ref);

        thread.jump(left);
    }
}

pub(crate) fn logical_not(thread: &mut JsThread) {
    let left = pop!(thread);

    thread.push_stack(!left.to_bool(&thread.realm));
    thread.step();
}

pub(crate) fn type_of(thread: &mut JsThread) {
    let left = pop!(thread);

    let constants = thread.realm.constants;
    let str = match left {
        RuntimeValue::Boolean(_) => constants.boolean,
        RuntimeValue::Null => constants.null,
        RuntimeValue::Object(obj) if obj.is_callable(&thread.realm.objects) => constants.function,
        RuntimeValue::Object(_) => constants.object,
        RuntimeValue::String(..) => constants.string,
        RuntimeValue::Float(_) => constants.number,
        RuntimeValue::Undefined => constants.undefined,
        _ => unreachable!("Unexpected runtime value for typeof"),
    };

    thread.push_stack(str);
    thread.step();
}

fn numeric_comparison_op(
    thread: &mut JsThread,
    str_op: impl FnOnce(&str, &str) -> bool,
    num_op: impl FnOnce(f64, f64) -> bool,
) {
    let left: RuntimeValue = pop!(thread);
    let right: RuntimeValue = pop!(thread);

    match (left, right) {
        (RuntimeValue::String(s1), RuntimeValue::String(s2)) => {
            let left = thread.realm.strings.get(s1).as_ref();
            let right = thread.realm.strings.get(s2).as_ref();
            thread.push_stack(str_op(left, right))
        }
        (left, right) => thread.push_stack(num_op(
            left.to_number(&thread.realm),
            right.to_number(&thread.realm),
        )),
    }

    thread.step();
}

pub(crate) fn in_operator(thread: &mut JsThread) {
    let left: RuntimeValue = pop!(thread);
    let right: RuntimeValue = pop!(thread);

    let obj = if let RuntimeValue::Object(obj) = right {
        obj
    } else {
        let type_error = thread.new_type_error(format!(
            "Cannot use 'in' operator to search for '{}' in {}",
            left, right
        ));
        return thread.throw(type_error);
    };

    let name = catch!(thread, left.to_string(thread));

    thread.push_stack(obj.has(&thread.realm.objects, name));
    thread.step();
}

pub(crate) fn greater_than(thread: &mut JsThread) {
    numeric_comparison_op(thread, |l, r| l > r, |l, r| l >= r)
}

pub(crate) fn greater_than_equal(thread: &mut JsThread) {
    numeric_comparison_op(thread, |l, r| l >= r, |l, r| l >= r)
}

pub(crate) fn less_than(thread: &mut JsThread) {
    numeric_comparison_op(thread, |l, r| l < r, |l, r| l >= r)
}

pub(crate) fn less_than_equal(thread: &mut JsThread) {
    numeric_comparison_op(thread, |l, r| l <= r, |l, r| l >= r)
}
