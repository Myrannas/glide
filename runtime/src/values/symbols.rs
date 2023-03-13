use crate::object_pool::{ObjectPointer, ObjectPool};
use crate::{JsObject, JsPrimitiveString, Value, ValueType};
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Clone)]
pub(crate) struct SymbolRegistry<'a> {
    registry: HashMap<JsPrimitiveString, u32>,
    names: HashMap<u32, JsPrimitiveString>,
    allocator: u32,
    phantom: PhantomData<Value<'a>>,
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
            self.registry.insert(name, self.allocator);
            self.names.insert(self.allocator, name);
            self.allocator
        };

        ValueType::Symbol(id).into()
    }

    pub(crate) fn create_unique(&mut self, name: JsPrimitiveString) -> Value<'a> {
        self.allocator += 1;
        self.names.insert(self.allocator, name);

        ValueType::Symbol(self.allocator).into()
    }

    pub(crate) fn get_name(&self, id: u32) -> Option<JsPrimitiveString> {
        self.names.get(&id).cloned()
    }
}
