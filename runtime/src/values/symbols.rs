use crate::{JsPrimitiveString, Value, ValueType};
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Clone)]
pub(crate) struct SymbolRegistry<'a> {
    registry: HashMap<JsPrimitiveString, JsSymbol>,
    names: HashMap<JsSymbol, JsPrimitiveString>,
    allocator: u32,
    phantom: PhantomData<Value<'a>>,
}

#[derive(Clone, PartialEq, Hash, Eq, Copy)]
pub struct JsSymbol(u32);

impl From<JsSymbol> for u64 {
    fn from(value: JsSymbol) -> u64 {
        value.0 as u64
    }
}

impl From<u32> for JsSymbol {
    fn from(value: u32) -> JsSymbol {
        JsSymbol(value)
    }
}

impl<'a> SymbolRegistry<'a> {
    pub(crate) fn new() -> SymbolRegistry<'a> {
        SymbolRegistry {
            registry: HashMap::new(),
            names: HashMap::new(),
            allocator: 0,
            phantom: PhantomData,
        }
    }

    pub(crate) fn create(&mut self, name: JsPrimitiveString) -> Value<'a> {
        let id = if let Some(id) = self.registry.get(&name) {
            *id
        } else {
            self.allocator += 1;

            let symbol = JsSymbol(self.allocator);
            self.registry.insert(name, symbol);
            self.names.insert(symbol, name);
            symbol
        };

        ValueType::Symbol(id).into()
    }

    pub(crate) fn create_unique(&mut self, name: JsPrimitiveString) -> Value<'a> {
        self.allocator += 1;

        let symbol = JsSymbol(self.allocator);
        self.names.insert(symbol, name);
        ValueType::Symbol(symbol).into()
    }

    pub(crate) fn get_name(&self, id: JsSymbol) -> Option<JsPrimitiveString> {
        self.names.get(&id).cloned()
    }
}
