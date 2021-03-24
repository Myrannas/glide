use crate::pool::{Pool, PoolPointer};
use ahash::{AHashMap, AHasher};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub(crate) struct StringValue {
    hash: u64,
    value: String,
}

impl Display for StringValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value)
    }
}

impl AsRef<str> for StringValue {
    fn as_ref(&self) -> &str {
        self.value.as_str()
    }
}

impl Hash for StringValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}

#[derive(Clone, PartialEq, Debug, Copy, Hash, Eq)]
pub struct StringPointer {
    inner: PoolPointer<StringValue>,
}

impl StringPointer {
    pub(crate) fn new(index: u32) -> StringPointer {
        StringPointer {
            inner: PoolPointer::new(index),
        }
    }
}

impl Display for StringPointer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("String@{}", self.inner))
    }
}

impl<'a> From<StringPointer> for u32 {
    fn from(value: StringPointer) -> Self {
        value.inner.into()
    }
}

#[derive(Clone)]
pub struct StringPool {
    pool: Pool<StringValue>,
    lookup: AHashMap<String, StringPointer>,
    natives: HashSet<StringPointer>,
}

impl StringPool {
    pub(crate) fn new() -> StringPool {
        StringPool {
            pool: Pool::new(),
            lookup: AHashMap::with_capacity(1024),
            natives: HashSet::new(),
        }
    }

    pub(crate) fn intern(&mut self, value: impl AsRef<str>) -> StringPointer {
        let value_str = value.as_ref();

        self.lookup.get(value_str).cloned().unwrap_or_else(|| {
            let mut hasher = AHasher::default();

            value_str.hash(&mut hasher);

            let id = self.pool.allocate(StringValue {
                hash: hasher.finish(),
                value: value_str.to_owned(),
            });

            let identifier = StringPointer { inner: id };

            self.lookup.insert(value_str.to_owned(), identifier);

            identifier
        })
    }

    pub(crate) fn intern_native(&mut self, value: &str) -> StringPointer {
        let pointer = self.intern(value);

        self.natives.insert(pointer);

        pointer
    }

    pub(crate) fn get(&self, pointer: StringPointer) -> &StringValue {
        self.pool.get(pointer.inner)
    }
}
