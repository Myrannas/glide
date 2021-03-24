use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

#[derive(Clone)]
pub struct Pool<V> {
    values: Vec<V>,
}

impl<T> Debug for Pool<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pool")
            .field("length", &self.values.len())
            .field("values", &self.values)
            .finish()
    }
}

type EntryId = u32;

#[derive(Debug)]
pub struct PoolPointer<V> {
    index: EntryId,
    phantom_data: std::marker::PhantomData<Pool<V>>,
}

impl<V> From<PoolPointer<V>> for u32 {
    fn from(value: PoolPointer<V>) -> Self {
        value.index
    }
}

impl<T> Display for PoolPointer<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.index))
    }
}

impl<T> Clone for PoolPointer<T> {
    fn clone(&self) -> Self {
        PoolPointer {
            index: self.index,
            phantom_data: self.phantom_data,
        }
    }
}

impl<T> Copy for PoolPointer<T> {}

impl<T> PartialEq for PoolPointer<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for PoolPointer<T> {}

impl<T> Hash for PoolPointer<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u32(self.index)
    }
}

impl<T> PoolPointer<T> {
    pub(crate) fn new(index: u32) -> Self {
        PoolPointer {
            index,
            phantom_data: PhantomData,
        }
    }
}

pub trait ObjectPool<V> {
    fn get(&self, index: PoolPointer<V>) -> &V;
    fn get_mut(&mut self, index: PoolPointer<V>) -> &mut V;
    fn allocate(&mut self, object: V) -> PoolPointer<V>;
}

impl<V> Pool<V> {
    pub(crate) fn new() -> Pool<V> {
        Pool {
            values: Vec::with_capacity(1024),
        }
    }

    pub(crate) fn get(&self, index: PoolPointer<V>) -> &V {
        &self.values[index.index as usize]
    }

    pub(crate) fn get_mut(&mut self, index: PoolPointer<V>) -> &mut V {
        &mut self.values[index.index as usize]
    }

    pub(crate) fn allocate(&mut self, object: V) -> PoolPointer<V> {
        let index = self.values.len();

        assert!(index < u32::MAX as usize);

        self.values.push(object);

        PoolPointer {
            index: index as u32,
            phantom_data: PhantomData,
        }
    }
}
