#[macro_use]
macro_rules! resolve {
    ($value:expr, $frame:ident) => {
        match Value::resolve($value, $frame) {
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
        match crate::Value::resolve($thread.pop_stack(), $thread) {
            Ok(value) => value,
            Err(err) => {
                $thread.throw(err);
                return;
            }
        }
    };

    ($thread:ident, $reason: literal) => {
        match crate::Value::resolve($thread.stack.pop().expect($reason).into(), $thread) {
            Ok(value) => value,
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
