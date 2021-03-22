#[macro_use]
macro_rules! resolve {
    ($value:expr, $frame:ident) => {
        match RuntimeValue::resolve($value, $frame) {
            Ok(value) => value,
            Err(err) => {
                $frame.throw(err);
                return;
            }
        }
    };
}

#[macro_use]
macro_rules! pop {
    ($thread:ident) => {
        match crate::RuntimeValue::resolve($thread.pop_stack(), $thread) {
            Ok(value) => value,
            Err(err) => {
                $thread.throw(err);
                return;
            }
        }
    };

    ($thread:ident, $reason: literal) => {
        match crate::RuntimeValue::resolve($thread.stack.pop().expect($reason), $thread) {
            Ok(value) => value.into(),
            Err(err) => {
                $thread.throw(err);
                return;
            }
        }
    };
}

#[macro_use]
macro_rules! catch {
    ($frame: ident, $value:expr) => {
        match $value {
            Ok(value) => value,
            Err(err) => {
                $frame.throw(err);
                return;
            }
        }
    };
}
