use crate::compiler::Chunk;
use crate::ops::{Context as JSContext, ContextAccess, ControlFlow, Instruction, RuntimeFrame};
use crate::value::{FunctionReference, JsObject, RuntimeValue};
use anyhow::{Context, Result};
use log::trace;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug)]
pub struct Module {
    pub init: Function,
}

impl Module {
    pub fn load<'a, 'b>(&'a self, global: &'b RuntimeValue<'a>) -> Result<(), ExecutionError<'a>> {
        let mut vec = Vec::with_capacity(4096);
        self.init
            .execute(None, &mut vec, 0..0, global, &RuntimeValue::Undefined)?;
        Ok(())
    }
}

pub struct Function {
    pub(crate) chunks: Vec<Chunk>,
    pub stack_size: usize,
    pub local_size: usize,
    pub functions: Vec<Function>,
    pub name: String,
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}()[locals: {}] {{ {:#?} }}",
            self.name, self.local_size, self.chunks
        ))
    }
}

#[derive(Debug, Error)]
pub enum ExecutionError<'a> {
    #[error("Runtime error")]
    Thrown(RuntimeValue<'a>),
    #[error("Encountered an internal error")]
    InternalError(&'a str),
}

impl Function {
    pub(crate) fn execute<'a, 'b, 'c>(
        &'a self,
        parent: Option<Rc<RefCell<JSContext<'a>>>>,
        stack: &'b mut Vec<RuntimeValue<'a>>,
        arguments: Range<usize>,
        global_this: &'c RuntimeValue<'a>,
        this: &'c RuntimeValue<'a>,
    ) -> Result<RuntimeValue<'a>, ExecutionError<'a>> {
        let mut frame = RuntimeFrame::<'a, 'b> {
            context: JSContext::with_parent(parent, self.local_size, this.clone()),
            stack,
            function: &self,
            global_this: global_this.clone(),
        };

        for (write_to, read_from) in arguments.enumerate().take(self.local_size) {
            frame
                .context
                .write(write_to, frame.stack[read_from].clone())
        }

        let mut chunk = &self.chunks[0];
        let mut chunk_length = chunk.instructions.len();
        let mut index = 0;
        while index < chunk_length {
            let Instruction { instr, constant } = &chunk.instructions[index];

            trace!("result: {:?}", frame);

            match instr(constant, &mut frame) {
                ControlFlow::Step => {
                    index += 1;
                }
                ControlFlow::Return(value) => return Ok(value),
                ControlFlow::Call {
                    target,
                    function: FunctionReference { function, context },
                    with_args,
                } => {
                    let stack_length = frame.stack.len();
                    let range = (stack_length - with_args)..stack_length;

                    let result = function.execute(
                        Some(context),
                        frame.stack,
                        range,
                        global_this,
                        &target,
                    )?;

                    trace!("{} = {:#?}", function.name, result);

                    frame.stack.push(result);
                }
                ControlFlow::Jump { chunk_index } => {
                    chunk = &self.chunks[chunk_index];
                    index = 0;
                    chunk_length = chunk.instructions.len();
                    continue;
                }
                ControlFlow::Throw(err) => {
                    return Err(ExecutionError::Thrown(err));
                }
            }
        }

        Err(ExecutionError::InternalError(&self.name))
    }
}
