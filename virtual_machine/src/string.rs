use crate::RuntimeValue;
use ahash::AHasher;
use colored::Colorize;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Eq)]
pub struct JsPrimitiveString {
    inner: Rc<String>,
    hash: u64,
}

impl Debug for JsPrimitiveString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.inner.as_str().yellow()))
    }
}

impl Display for JsPrimitiveString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.inner.as_ref()))
    }
}

impl Hash for JsPrimitiveString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl<'a> PartialEq for JsPrimitiveString {
    fn eq(&self, other: &Self) -> bool {
        self.inner.as_ref() == other.inner.as_ref()
    }
}

impl<T: Into<String>> From<T> for JsPrimitiveString {
    fn from(value: T) -> Self {
        let str = value.into();
        let mut hash = AHasher::default();
        hash.write(str.as_bytes());

        JsPrimitiveString {
            inner: Rc::new(str),
            hash: hash.finish(),
        }
    }
}

impl<'a> From<JsPrimitiveString> for RuntimeValue<'a> {
    fn from(string: JsPrimitiveString) -> Self {
        RuntimeValue::String(string)
    }
}

impl AsRef<str> for JsPrimitiveString {
    fn as_ref(&self) -> &str {
        self.inner.as_ref()
    }
}
