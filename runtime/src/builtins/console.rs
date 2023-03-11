use crate::debugging::X;
use crate::{JsThread, Value};
use builtin::{named, prototype, varargs};
use log::{log, Level};
use std::fmt::{Display, Formatter, Pointer, Write};

pub(crate) struct JsConsole<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

struct PrintList<'a, 'b, 'c> {
    args: Vec<X<'a, 'b, 'c>>,
}

impl<'a, 'b, 'c> Display for PrintList<'a, 'b, 'c> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;

        for arg in &self.args {
            if !first {
                f.write_char(' ')?;
            } else {
                first = false;
            }
            arg.fmt(f)?
        }

        Ok(())
    }
}

#[prototype]
#[named("console")]
impl<'a, 'b> JsConsole<'a, 'b> {
    #[varargs]
    fn log(thread: &mut JsThread<'a>, args: Vec<Value<'a>>) {
        let list = PrintList {
            args: args.iter().map(|v| X::from(v, &thread.realm)).collect(),
        };

        log!(Level::Info, "{}", list);
    }
}
