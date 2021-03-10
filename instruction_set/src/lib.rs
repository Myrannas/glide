#![forbid(unsafe_code)]

extern crate serde;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

type ChunkId = usize;
type LocalId = usize;
type Frame = usize;
type Atom = usize;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Instruction {
    Truncate,
    Add,
    Subtract,
    Divide,
    Modulo,
    Multiply,

    LoadConstant { constant: Constant },
    LoadEnvironmental { environmental: Environmental },

    Return,
    ReturnConstant { constant: Constant },
    Call { args_count: usize },
    CallNew { args_count: usize },
    Jump { to: ChunkId },
    CompareJump { if_true: ChunkId, if_false: ChunkId },

    SetLocal { local: LocalId },
    SetCapture { local: LocalId, frame: Frame },
    SetNamed { name: Atom },
    Set,

    GetLocal { local: LocalId },
    GetCapture { local: LocalId, frame: Frame },
    GetNamed { name: Atom },
    GetFunction { function: usize },
    Get,

    Delete,
    DeleteNamed { name: Atom },
    DeleteLocal { local: LocalId },

    StrictEqualTo,
    NotStrictEqualTo,
    EqualTo,
    NotEqualTo,
    TypeOf,
    LogicalNot,
    Neg,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    LogicalOr { left: ChunkId, right: ChunkId },
    LogicalAnd { left: ChunkId, right: ChunkId },
    ThrowValue,
    RightShiftUnsigned,
    RightShift,
    LeftShift,
    InstanceOf,
    Increment { by: f64 },
    Catch { chunk: ChunkId },
    DropCatch { chunk: ChunkId },
    Duplicate,
    Resolve,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Constant {
    Null,
    Undefined,
    Float(f64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum LocalInit {
    Constant(Constant),
    Function(usize),
}

impl Default for LocalInit {
    fn default() -> Self {
        LocalInit::Constant(Constant::Undefined)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Environmental {
    GlobalThis,
    This,
    NewObject,
}

pub type Chunk = Vec<Instruction>;

#[derive(Debug, Clone)]
pub struct Local {
    pub name: String,
    pub local: LocalId,
    pub frame: Frame,
    pub argument: bool,
}

#[derive(Debug)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub atoms: Vec<String>,
    pub functions: Vec<Function>,
    pub stack_size: usize,
    pub args_size: usize,
    pub name: Option<String>,
    pub locals_init: Vec<LocalInit>,

    pub local_size: usize,
    pub locals: Vec<Local>,
}

pub struct Module {
    pub init: Function,
}
