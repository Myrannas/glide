use crate::JsThread;

pub(crate) fn left_shift(thread: &mut JsThread) {
    let left: i32 = pop!(thread);
    let right: u32 = pop!(thread);

    thread.push_stack((left << right) as f64);
    thread.step();
}

pub(crate) fn right_shift(thread: &mut JsThread) {
    let left: i32 = pop!(thread);
    let right: u32 = pop!(thread);

    thread.push_stack((left >> right) as f64);
    thread.step();
}

pub(crate) fn right_shift_unsigned(thread: &mut JsThread) {
    let value: i32 = pop!(thread);
    let shift: u32 = pop!(thread);
    let shift = shift & &0x1F;

    thread.push_stack((value >> shift) as f64);
    thread.step();
}
