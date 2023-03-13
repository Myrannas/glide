use crate::object_pool::ObjectPool;
use crate::result::JsResult;
use crate::string_pool::StringPool;
use crate::values::symbols::SymbolRegistry;
use crate::{ExecutionError, JsObject, JsThread, Value, ValueType};
use builtin::{callable, constant, named, prototype};

pub(crate) struct JsSymbol<'a, 'b> {
    target: Value<'a>,
    thread: &'b mut JsThread<'a>,
}

#[prototype]
#[named("Symbol")]
impl<'a, 'b> JsSymbol<'a, 'b> {
    #[callable]
    fn unique(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let name = value.to_string(thread)?;

        Ok(thread.realm.symbols.create_unique(name))
    }

    #[named("for")]
    fn for_value(thread: &mut JsThread<'a>, value: Value<'a>) -> JsResult<'a> {
        let name = value.to_string(thread)?;

        Ok(thread.realm.symbols.create(name))
    }

    #[named("valueOf")]
    fn value_of(&self) -> JsResult<'a> {
        match self.target.get_type() {
            ValueType::Symbol(id) => Ok(ValueType::Symbol(id).into()),
            _ => Err(ExecutionError::TypeError(
                "Cannot convert value into a symbol".to_string(),
            )),
        }
    }

    #[constant]
    fn iterator(
        pool: &mut ObjectPool<'a>,
        strings: &mut StringPool,
        symbols: &mut SymbolRegistry<'a>,
    ) -> Value<'a> {
        let name = strings.intern_native("iterator");

        symbols.create_unique(name)
    }
}
