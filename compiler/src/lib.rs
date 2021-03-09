#![forbid(unsafe_code)]

mod compiler;
mod parser;
pub mod result;

extern crate anyhow;
extern crate instruction_set;
extern crate log;

pub use compiler::{compile, CompilerOptions};
pub use instruction_set::Module;
pub use parser::{parse_input, ParsedModule};
pub use result::{CompilerError, InternalError, SyntaxError};

#[cfg(feature = "eval")]
pub use compiler::compile_eval;
