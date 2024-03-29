use crate::{ExecutionError, JsThread, Realm, Value};
use better_any::TidAble;
use std::cell::{Ref, RefMut};

pub(crate) mod arguments;
pub(crate) mod array;
pub(crate) mod console;
pub(crate) mod errors;
pub(crate) mod function;
pub(crate) mod map;
pub(crate) mod math;
pub(crate) mod number;
pub(crate) mod objects;
pub(crate) mod promise;
pub(crate) mod prototype;
pub(crate) mod set;
pub(crate) mod string;

pub(crate) mod boolean;
#[cfg(feature = "runtime_time")]
pub(crate) mod date;
#[cfg(feature = "eval")]
pub(crate) mod eval;
pub(crate) mod regexp;
pub(crate) mod symbol;

#[inline]
pub(crate) fn native_target<'a, 'b, T: TidAble<'a>>(
    target: Value<'a>,
    realm: &'b Realm<'a>,
) -> Result<Ref<'b, T>, ExecutionError<'a>> {
    {
        target
            .as_object(realm)?
            .get_native_handle::<T>(&realm.objects)
            .ok_or_else(|| ExecutionError::TypeError("Invalid Target".to_string()))
    }
}

#[inline]
pub(crate) fn native_target_mut<'a, 'b, T: TidAble<'a>>(
    target: Value<'a>,
    realm: &'b mut Realm<'a>,
) -> Result<RefMut<'b, &'a mut T>, ExecutionError<'a>> {
    {
        target
            .as_object(realm)?
            .mut_native_handle::<T>(&mut realm.objects)
            .ok_or_else(|| ExecutionError::TypeError("Invalid Target".to_string()))
    }
}
