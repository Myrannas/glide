use crate::{catch, pop, JsThread};

pub(crate) fn left_shift(thread: &mut JsThread) {
    let left = catch!(thread, pop!(thread).to_i32(&thread.realm));
    let right: u32 = catch!(thread, pop!(thread).to_u32(&thread.realm));

    thread.push_stack(left << right);
    thread.step();
}

pub(crate) fn right_shift(thread: &mut JsThread) {
    let left: i32 = catch!(thread, pop!(thread).to_i32(&thread.realm));
    let right: u32 = catch!(thread, pop!(thread).to_u32(&thread.realm));

    thread.push_stack(left >> right);
    thread.step();
}

pub(crate) fn right_shift_unsigned(thread: &mut JsThread) {
    let value: i32 = catch!(thread, pop!(thread).to_i32(&thread.realm));
    let shift: u32 = catch!(thread, pop!(thread).to_u32(&thread.realm));
    let shift = shift & 0x1F;

    thread.push_stack(value >> shift);
    thread.step();
}
