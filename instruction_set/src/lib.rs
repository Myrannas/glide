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
    Add { times: u8 },
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
    SetPrivate { index: PrivateValue },
    Set,

    GetLocal { local: LocalId },
    GetCapture { local: LocalId, frame: Frame },
    GetNamed { name: Atom },
    GetFunction { function: usize },
    GetClass { class: usize, extends: bool },
    GetPrivate { index: PrivateValue },
    Get,

    Delete,
    DeleteNamed { name: Atom },
    DeleteLocal { local: LocalId },
    Debug { entire_stack: bool },

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
    Exponential,
    Increment { by: f64, pre: bool },
    Catch { chunk: ChunkId },
    DropCatch { chunk: ChunkId },
    Duplicate,
    Resolve,
    In,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub enum Constant {
    Null,
    Undefined,
    Float(f64),
    Boolean(bool),
    Atom(Atom),
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
pub struct PrivateField {
    pub id: usize,
    pub name: String,
    pub declared: bool,
}

#[derive(Debug)]
pub struct Class {
    pub construct: Option<Function>,
    pub name: Option<Atom>,

    pub atoms: Vec<String>,

    pub methods: Vec<Function>,
    pub static_methods: Vec<Function>,
    pub private_fields: usize,
    pub properties: Vec<Property>,
    pub static_properties: Vec<Property>,
}

#[derive(Debug)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub atoms: Vec<String>,
    pub functions: Vec<Function>,
    pub classes: Vec<Class>,
    pub stack_size: usize,
    pub args_size: usize,
    pub name: Option<String>,
    pub locals_init: Vec<LocalInit>,

    pub local_size: usize,
    pub locals: Vec<Local>,
}

#[derive(Debug)]
pub struct Property {
    pub name: String,
    pub getter: Option<Function>,
    pub setter: Option<Function>,
}

impl Default for Function {
    fn default() -> Self {
        Function {
            instructions: vec![],
            atoms: vec![],
            functions: vec![],
            classes: vec![],
            stack_size: 0,
            args_size: 0,
            name: None,
            locals_init: vec![],
            local_size: 0,
            locals: vec![],
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub init: Function,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PrivateValue(pub usize);
