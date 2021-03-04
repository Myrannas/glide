use crate::compiler::{Chunk, FrameLocals};
use crate::debugging::{DebugRepresentation, Renderer};
use crate::object::Object;
use crate::ops::{
    CallStack, ControlFlow, Instruction, JsContext as JSContext, JsContext, RuntimeFrame,
};
use crate::result::{InternalError, JsResult};
use crate::value::{make_arguments, CustomFunctionReference, FunctionReference, RuntimeValue};
use crate::ExecutionError;
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
    // pub fn load<'a, 'b>(&'a self, global: &'b Object<'a>) -> JsResult<()> {
    //     let mut vec = Vec::with_capacity(4096);
    //     self.init
    //         .execute(None, &mut vec, 0..0, None, global, &RuntimeValue::Undefined)?;
    //     Ok(())
    // }
}

pub(crate) struct FunctionInner {
    pub(crate) chunks: Vec<Chunk>,
    pub(crate) stack_size: usize,
    pub(crate) local_size: usize,
    pub(crate) functions: Vec<Function>,
    pub(crate) name: String,
    pub(crate) locals: Rc<RefCell<FrameLocals>>,
}

impl From<FunctionInner> for Function {
    fn from(inner: FunctionInner) -> Self {
        Function {
            inner: Rc::new(inner),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    inner: Rc<FunctionInner>,
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.render(&mut Renderer::debug(f, 3))
    }
}

impl DebugRepresentation for Function {
    fn render(&self, f: &mut Renderer) -> std::fmt::Result {
        f.function(&self.inner.name)?;

        f.start_internal("FUNCTION")?;

        f.internal_key("instructions")?;

        f.new_line()?;

        for (i, chunk) in self.inner.chunks.iter().enumerate() {
            f.internal_index(i)?;
            f.new_line()?;

            for instruction in &chunk.instructions {
                Instruction::render(instruction, f)?;
                f.new_line()?;
            }
        }

        f.end_internal()?;

        Ok(())
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl Function {
    pub fn name(&self) -> &str {
        &self.inner.name
    }

    pub(crate) fn inner(&self) -> &FunctionInner {
        &self.inner
    }

    pub(crate) fn child_function(&self, index: usize) -> &Function {
        &self.inner.functions[index]
    }

    // pub(crate) fn execute<'a, 'b, 'c, 'd, 'e>(
    //     &'d self,
    //     parent: Option<JSContext<'a>>,
    //     stack: &'b mut Vec<RuntimeValue<'a>>,
    //     arguments: Range<usize>,
    //     call_stack: Option<Rc<CallStack>>,
    //     global_this: &'c Object<'a>,
    //     this: &'c RuntimeValue<'a>,
    // ) -> JsResult<'a> {
    //     // println!("{:#?}", self);
    //
    //     let mut frame = RuntimeFrame::<'a, 'b> {
    //         context: JSContext::with_parent(parent, self.inner.local_size, this.clone()),
    //         stack,
    //         global_this: global_this.clone(),
    //         call_stack: Rc::new(CallStack {
    //             parent: call_stack,
    //             function: self.clone(),
    //         }),
    //     };
    //
    //     for (write_to, read_from) in arguments.clone().enumerate().take(self.inner.local_size) {
    //         frame
    //             .context
    //             .write(write_to + 1, frame.stack[read_from].clone())
    //     }
    //
    //     frame.context.write(
    //         0,
    //         make_arguments(arguments.map(|i| frame.stack[i].clone()).collect()),
    //     );
    //
    //     let mut current_chunk_index = 0;
    //     let mut chunk = &self.inner.chunks[current_chunk_index];
    //     let mut chunk_length = chunk.instructions.len();
    //     let mut index = 0;
    //     while index < chunk_length {
    //         let Instruction { instr, constant } = &chunk.instructions[index];
    //
    //         // println!("{:?} \n result: {:?}", &chunk.instructions[index], frame);
    //
    //         let result = instr(constant, &mut frame);
    //
    //         match result {
    //             Ok(ControlFlow::Step) => {
    //                 index += 1;
    //             }
    //             Ok(ControlFlow::Return(value)) => {
    //                 // let to_cleanup = frame.stack.len() - start_stack;
    //                 //
    //                 // if to_cleanup > 0 {
    //                 //     for _ in 0..to_cleanup {
    //                 //         frame.stack.pop();
    //                 //     }
    //                 // }
    //
    //                 return Ok(value);
    //             }
    //             Ok(ControlFlow::Call {
    //                 target,
    //                 function: CustomFunctionReference { function, context },
    //                 with_args,
    //                 new,
    //             }) => {
    //                 let stack_length = frame.stack.len();
    //                 let range = (stack_length - with_args)..stack_length;
    //
    //                 let result = function.execute(
    //                     Some(context),
    //                     frame.stack,
    //                     range,
    //                     Some(frame.call_stack.clone()),
    //                     global_this,
    //                     &target,
    //                 )?;
    //
    //                 trace!("{} = {:#?}", function.inner.name, result);
    //
    //                 if new {
    //                     frame.stack.push(target)
    //                 } else {
    //                     frame.stack.push(result);
    //                 }
    //
    //                 index += 1;
    //             }
    //             Ok(ControlFlow::Jump { chunk_index }) => {
    //                 chunk = &self.inner.chunks[chunk_index];
    //                 current_chunk_index = chunk_index;
    //                 index = 0;
    //                 chunk_length = chunk.instructions.len();
    //                 continue;
    //             }
    //             Err(err) => return Err(err.fill_stack_trace(frame.call_stack.stack_trace())),
    //         }
    //     }
    //
    //     InternalError::new(
    //         format!(
    //             "Execution error - hit end of function ({}:{})",
    //             current_chunk_index, index
    //         ),
    //         frame.call_stack.stack_trace(),
    //     )
    //     .into()
    // }
}

struct StackFrame<'a> {
    current_function: Function,
    current_chunk_index: usize,
    index: usize,
    context: JsContext<'a>,
}

impl<'a> StackFrame<'a> {
    fn get_chunk(&self) -> &Chunk {
        &self.current_function.inner.chunks[self.current_chunk_index]
    }
}

pub struct JsThread<'a> {
    pub(crate) stack: Vec<RuntimeValue<'a>>,
    call_stack: Vec<StackFrame<'a>>,
    current_frame: StackFrame<'a>,
    pub(crate) global_this: Object<'a>,
}

impl<'a> JsThread<'a> {
    pub(crate) fn current_context(&self) -> &JsContext<'a> {
        &self.current_frame.context
    }

    pub(crate) fn current_function(&self) -> &Function {
        &self.current_frame.current_function
    }

    pub fn new(function: Function, global_this: Object<'a>) -> JsThread<'a> {
        let local_size = function.inner.local_size;

        JsThread {
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(128),
            current_frame: StackFrame {
                current_function: function,
                current_chunk_index: 0,
                index: 0,
                context: JSContext::with_parent(
                    None,
                    local_size,
                    RuntimeValue::Object(global_this.clone()),
                ),
            },
            global_this,
        }
    }

    pub(crate) fn step(&mut self) {
        self.current_frame.index += 1;
    }

    pub(crate) fn return_value(&mut self, value: RuntimeValue<'a>) {
        if let Some(parent) = self.call_stack.pop() {
            self.current_frame = parent;
            self.stack.push(value);
        }

        self.current_frame.index += 1;
    }

    pub(crate) fn jump(&mut self, chunk_index: usize) {
        self.current_frame.current_chunk_index = chunk_index;
        self.current_frame.index = 0;
    }

    pub(crate) fn throw(&mut self, error: impl Into<ExecutionError<'a>>) {}

    fn next_instruction(&self) -> Instruction {
        self.current_frame.get_chunk().instructions[self.current_frame.index].clone()
    }

    pub(crate) fn call(
        &mut self,
        target: RuntimeValue<'a>,
        CustomFunctionReference { function, context }: CustomFunctionReference<'a>,
        args: usize,
        new: bool,
    ) {
        let new_context = JsContext::with_parent(
            Some(self.current_frame.context.clone()),
            function.inner.local_size,
            target.clone(),
        );

        let stack_length = self.stack.len();
        let arguments = (stack_length - args)..stack_length;

        for (write_to, read_from) in arguments
            .clone()
            .enumerate()
            .take(function.inner.local_size)
        {
            new_context.write(write_to + 1, self.stack[read_from].clone())
        }

        new_context.write(
            0,
            make_arguments(arguments.map(|i| self.stack[i].clone()).collect()),
        );

        let new_frame = StackFrame {
            current_function: function,
            current_chunk_index: 0,
            index: 0,
            context: new_context,
        };

        self.call_stack
            .push(std::mem::replace(&mut self.current_frame, new_frame));
    }

    pub fn run(&mut self) -> JsResult<'a, ()> {
        while self.current_frame.index < self.current_frame.get_chunk().instructions.len() {
            let Instruction { instr, constant } = self.next_instruction();

            // println!("{:?} \n result: {:?}", &chunk.instructions[index], frame);

            instr(&constant, self);
        }

        Ok(())
    }
}
