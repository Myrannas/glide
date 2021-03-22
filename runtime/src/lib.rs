#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::pedantic::large_types_passed_by_value,
    clippy::restriction::clone_on_ref_ptr
)]
#![allow(
    clippy::module_name_repetitions,
    clippy::missing_docs_in_private_items,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::cast_precision_loss,
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation
)]

mod builtins;
mod context;
mod debugging;
mod object_pool;
mod ops;
mod pool;
mod primordials;
mod result;
mod string_pool;
mod values;
mod vm;

extern crate ahash;
extern crate anyhow;
extern crate builtin;
extern crate colored;
extern crate instruction_set;
extern crate rand;

pub use primordials::Realm;
pub use result::{ExecutionError, InternalError, SyntaxError};
pub use values::function::{BuiltIn, JsFunction};
pub use values::object::{FunctionObject, JsObject};
pub use values::string::JsPrimitiveString;
pub use values::value::RuntimeValue;
pub use vm::JsThread;
