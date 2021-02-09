use crate::compiler::Chunk;
use crate::ops::{Context, ContextAccess, ControlFlow, Instruction, RuntimeFrame};
use crate::value::{FunctionReference, RuntimeValue};
use log::trace;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use std::rc::Rc;

#[derive(Debug)]
pub struct Module {
    pub init: Function,
}

impl Module {
    pub(crate) fn load(&self) -> Result<(), ExecutionError> {
        let mut vec = Vec::with_capacity(4096);
        self.init.execute(None, &mut vec, 0..0)?;
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
        f.write_fmt(format_args!("{}() {{ {:#?} }}", self.name, self.chunks))
    }
}

#[derive(Debug)]
pub enum ExecutionError {}

impl Function {
    pub(crate) fn execute<'a, 'b>(
        &'a self,
        parent: Option<Rc<RefCell<Context<'a>>>>,
        stack: &'b mut Vec<RuntimeValue<'a>>,
        arguments: Range<usize>,
    ) -> Result<RuntimeValue<'a>, ExecutionError> {
        let mut frame = RuntimeFrame::<'a, 'b> {
            context: Context::with_parent(parent, self.local_size),
            stack,
            function: &self,
        };

        for (write_to, read_from) in arguments.enumerate() {
            frame
                .context
                .write(write_to, frame.stack[read_from].clone())
        }

        let mut chunk = &self.chunks[0];
        let mut chunk_length = chunk.instructions.len();
        let mut index = 0;
        while index < chunk_length {
            let Instruction { instr, constant } = &chunk.instructions[index];

            // trace!("result: {:?}", result);

            match instr(constant, &mut frame) {
                ControlFlow::Step => {
                    index += 1;
                }
                ControlFlow::Return(value) => return Ok(value),
                ControlFlow::Call {
                    function: FunctionReference { function, context },
                    with_args,
                } => {
                    let stack_length = frame.stack.len();
                    let range = (stack_length - with_args)..stack_length;
                    let result = function.execute(Some(context), frame.stack, range)?;

                    trace!("{} = {:#?}", function.name, result);

                    frame.stack.push(result);
                }
                ControlFlow::Jump { chunk_index } => {
                    chunk = &self.chunks[chunk_index];
                    index = 0;
                    chunk_length = chunk.instructions.len();
                    continue;
                }
            }
        }

        panic!("Block finished abnormally")
    }
}
