use crate::string_pool::StringPointer;
use crate::RuntimeValue;

pub type JsPrimitiveString = StringPointer;

impl<'a> From<JsPrimitiveString> for RuntimeValue<'a> {
    fn from(string: JsPrimitiveString) -> Self {
        RuntimeValue::String(string)
    }
}
