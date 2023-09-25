use crate::pool::PoolPointer;
use ahash::{AHashMap, AHasher};
use stash::{Index, Stash};
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
    pub(crate) fn new(pool_pointer: PoolPointer<StringValue>) -> StringPointer {
        StringPointer {
            inner: pool_pointer,
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

impl Index for StringPointer {
    fn from_usize(idx: usize) -> Self {
        StringPointer {
            inner: PoolPointer::from_usize(idx),
        }
    }

    fn into_usize(self) -> usize {
        self.inner.into_usize()
    }
}

#[derive(Clone)]
pub struct StringPool {
    pool: Stash<StringValue, StringPointer>,
    lookup: AHashMap<String, StringPointer>,
    natives: HashSet<StringPointer>,
}

impl StringPool {
    pub(crate) fn new() -> StringPool {
        StringPool {
            pool: Stash::default(),
            lookup: AHashMap::with_capacity(1024),
            natives: HashSet::new(),
        }
    }

    pub(crate) fn intern(&mut self, value: impl AsRef<str>) -> StringPointer {
        let value_str = value.as_ref();

        self.lookup.get(value_str).cloned().unwrap_or_else(|| {
            let mut hasher = AHasher::default();

            value_str.hash(&mut hasher);

            let id = self.pool.put(StringValue {
                hash: hasher.finish(),
                value: value_str.to_owned(),
            });

            self.lookup.insert(value_str.to_owned(), id);

            id
        })
    }

    pub(crate) fn manipulate(
        &mut self,
        pointer: StringPointer,
        function: impl Fn(&str) -> &str,
    ) -> StringPointer {
        let current_strings = &self.pool[pointer];

        let result = function(current_strings.as_ref());

        if let Some(existing) = self.lookup.get(result) {
            *existing
        } else {
            let owned_string = result.to_owned();
            self.intern(owned_string)
        }
    }

    pub(crate) fn manipulate_owned(
        &mut self,
        pointer: StringPointer,
        function: impl Fn(&str) -> String,
    ) -> StringPointer {
        let current_strings = &self.pool[pointer];

        let result = function(current_strings.as_ref());

        if let Some(existing) = self.lookup.get(&result) {
            *existing
        } else {
            let owned_string = result.to_owned();
            self.intern(owned_string)
        }
    }

    pub(crate) fn intern_native(&mut self, value: &str) -> StringPointer {
        let pointer = self.intern(value);

        self.natives.insert(pointer);

        pointer
    }

    pub(crate) fn get(&self, pointer: StringPointer) -> &StringValue {
        &self.pool[pointer]
    }
}
