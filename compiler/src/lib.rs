mod compiler;
mod ops;
mod parser;
pub mod value;
mod vm;
mod debugging;

extern crate anyhow;
extern crate log;
extern crate pretty_env_logger;
extern crate thiserror;

pub use compiler::{compile, CompilerOptions};
pub use parser::parse_input;
pub use vm::Module;
