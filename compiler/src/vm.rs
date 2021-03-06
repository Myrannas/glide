use crate::compiler::{Chunk, FrameLocals};
use crate::debugging::{DebugRepresentation, Renderer};
use crate::object::{Object, ObjectMethods};
use crate::ops::{CallStack, Instruction, JsContext as JSContext, JsContext, RuntimeFrame};
use crate::primordials::GlobalThis;
use crate::result::{InternalError, JsResult, Stack, StackTraceFrame};
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
}

#[derive(Debug)]
struct StackFrame<'a> {
    current_function: Function,
    current_chunk_index: usize,
    index: usize,
    context: JsContext<'a>,
    stack_size: usize,
}

#[derive(Debug)]
struct CatchFrame {
    chunk: usize,
    stack_frame: usize,
}

impl<'a> From<&mut JsThread<'a>> for Stack {
    fn from(thread: &mut JsThread<'a>) -> Self {
        let mut call_stack: Vec<StackTraceFrame> = thread
            .call_stack
            .iter()
            .map(|frame| StackTraceFrame {
                function: frame.current_function.name().to_owned(),
                chunk: frame.current_chunk_index,
                offset: frame.index,
            })
            .collect();

        call_stack.push(StackTraceFrame {
            function: thread.current_frame.current_function.name().to_owned(),
            chunk: thread.current_frame.current_chunk_index,
            offset: thread.current_frame.index,
        });

        call_stack.reverse();

        Stack {
            entries: call_stack,
        }
    }
}

impl<'a> StackFrame<'a> {
    fn get_chunk(&self) -> &Chunk {
        &self.current_function.inner.chunks[self.current_chunk_index]
    }
}

pub struct JsThread<'a> {
    pub(crate) stack: Vec<RuntimeValue<'a>>,
    call_stack: Vec<StackFrame<'a>>,
    catch_stack: Vec<CatchFrame>,
    current_frame: StackFrame<'a>,
    pub(crate) global_this: GlobalThis<'a>,
    error: Option<ExecutionError<'a>>,
    cost_limit: Option<usize>,
    call_stack_limit: usize,
}

impl<'a> JsThread<'a> {
    pub(crate) fn current_context(&self) -> &JsContext<'a> {
        &self.current_frame.context
    }

    pub(crate) fn current_function(&self) -> &Function {
        &self.current_frame.current_function
    }

    pub fn new(function: Function, global_this: GlobalThis<'a>) -> JsThread<'a> {
        let local_size = function.inner.local_size;

        JsThread {
            stack: Vec::with_capacity(1024),
            call_stack: Vec::new(),
            catch_stack: Vec::new(),
            current_frame: StackFrame {
                current_function: function,
                current_chunk_index: 0,
                index: 0,
                stack_size: 0,
                context: JSContext::with_parent(
                    None,
                    local_size,
                    RuntimeValue::Object(global_this.global_this.clone()),
                ),
            },
            global_this,
            error: None,
            cost_limit: None,
            call_stack_limit: 1024,
        }
    }

    pub fn set_cost_limit(&mut self, limit: usize) {
        self.cost_limit = Some(limit)
    }

    pub(crate) fn step(&mut self) {
        self.current_frame.index += 1;
    }

    pub(crate) fn return_value(&mut self, value: RuntimeValue<'a>) {
        if let Some(parent) = self.call_stack.pop() {
            self.current_frame = parent;
            self.stack.push(value);
        }

        self.stack.truncate(self.current_frame.stack_size);
        self.current_frame.index += 1;
    }

    pub(crate) fn jump(&mut self, chunk_index: usize) {
        if let Some(limit) = self.cost_limit {
            if limit > 0 {
                self.cost_limit = Some(limit - 1)
            } else {
                return self.throw(InternalError::new_stackless("Hit execution limit"));
            }
        }

        self.current_frame.current_chunk_index = chunk_index;
        self.current_frame.index = 0;
    }

    pub(crate) fn throw(&mut self, error: impl Into<ExecutionError<'a>>) {
        let stack: Stack = self.into();
        let error = error.into().fill_stack_trace(stack);

        match (error, self.catch_stack.pop()) {
            (ExecutionError::Thrown(error, ..), Some(CatchFrame { stack_frame, chunk })) => {
                self.call_stack.truncate(stack_frame + 1);

                if let Some(call) = self.call_stack.pop() {
                    self.current_frame = call;
                    self.stack.truncate(self.current_frame.stack_size);
                }

                self.current_frame.current_chunk_index = chunk;
                self.current_frame.index = 0;
                self.stack.push(error);
            }
            (error, ..) => {
                self.call_stack.truncate(1);
                if let Some(call) = self.call_stack.pop() {
                    self.current_frame = call;
                    self.stack.truncate(self.current_frame.stack_size);
                }

                self.current_frame.index = usize::MAX;
                self.error = Some(error);
            }
        }
    }

    pub(crate) fn catch(&mut self, chunk: usize) {
        self.catch_stack.push(CatchFrame {
            chunk,
            stack_frame: self.call_stack.len(),
        });
        self.step();
    }

    pub(crate) fn uncatch(&mut self) {
        self.catch_stack.pop();
    }

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
        if self.call_stack.len() > self.call_stack_limit {
            return self.throw(InternalError::new_stackless("Stack overflow"));
        }

        if let Some(limit) = self.cost_limit {
            if limit > 0 {
                self.cost_limit = Some(limit - 1)
            } else {
                return self.throw(InternalError::new_stackless("Hit execution limit"));
            }
        }

        let new_context = JsContext::with_parent(
            Some(context.clone()),
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

        self.current_frame.stack_size = self.stack.len();
        let new_frame = StackFrame {
            current_function: function,
            current_chunk_index: 0,
            index: 0,
            context: new_context,
            stack_size: 0,
        };

        self.call_stack
            .push(std::mem::replace(&mut self.current_frame, new_frame));
    }

    pub fn run(&mut self) -> JsResult<'a, ()> {
        while self.current_frame.index < self.current_frame.get_chunk().instructions.len() {
            let Instruction { instr, constant } = self.next_instruction();

            // println!("{:?} \n result: {:?}", &chunk.instructions[index], frame);

            // println!("{:?}", self.next_instruction());

            instr(&constant, self);
        }

        if let Some(err) = std::mem::replace(&mut self.error, None) {
            Err(err)
        } else {
            Ok(())
        }
    }
}