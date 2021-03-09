#![forbid(unsafe_code)]

mod builtins;
mod context;
mod debugging;
mod function;
mod object;
mod ops;
mod primordials;
mod result;
mod string;
mod value;
mod vm;

extern crate ahash;
extern crate anyhow;
extern crate builtin;
extern crate colored;
extern crate instruction_set;
extern crate rand;

pub use function::{BuiltIn, JsFunction};
pub use object::JsObject;
pub use primordials::GlobalThis;
pub use result::{ExecutionError, InternalError};
pub use value::RuntimeValue;
pub use vm::JsThread;
