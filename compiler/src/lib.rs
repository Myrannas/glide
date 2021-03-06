mod compiler;
mod debugging;
mod object;
mod ops;
mod parser;
mod primordials;
mod result;
pub mod value;
mod vm;

extern crate anyhow;
extern crate arr_macro;
extern crate log;
extern crate pretty_env_logger;
extern crate thiserror;

pub use compiler::{compile, CompilerOptions};
pub use object::Object;
pub use parser::{parse_input, ParsedModule};
pub use primordials::GlobalThis;
pub use result::{ExecutionError, InternalError, StaticExecutionError, SyntaxError};
pub use vm::{JsThread, Module};
