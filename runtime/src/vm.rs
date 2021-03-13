use crate::context::JsContext;
use crate::ops::Operand;
use crate::primordials::Realm;
use crate::result::{InternalError, JsResult, Stack, StackTraceFrame};
use crate::values::function::{CustomFunctionReference, FunctionReference};
use crate::values::value::{make_arguments, RuntimeValue};
use crate::{ExecutionError, JsFunction, JsObject};
use instruction_set::Instruction;
use log::trace;
use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct Module {
    pub init: JsFunction,
}

impl Module {
    // pub fn load<'a, 'b>(&'a self, global: &'b Object<'a>) -> JsResult<()> {
    //     let mut vec = Vec::with_capacity(4096);
    //     self.init
    //         .execute(None, &mut vec, 0..0, None, global, &RuntimeValue::Undefined)?;
    //     Ok(())
    // }
}

const END_OF_FRAME: usize = usize::MAX - 1;

#[derive(Clone)]
struct StackFrame<'a> {
    current_function: JsFunction,
    index: usize,
    context: JsContext<'a>,
    stack_size: usize,
    is_new: bool,
}

impl<'a> Debug for StackFrame<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} ({})",
            self.current_function.name(),
            self.index
        ))
    }
}

#[derive(Debug)]
struct CatchFrame {
    instruction_index: usize,
    stack_frame: usize,
}

impl<'a> From<&mut JsThread<'a>> for Stack {
    fn from(thread: &mut JsThread<'a>) -> Self {
        let mut call_stack: Vec<StackTraceFrame> = thread
            .call_stack
            .iter()
            .map(|frame| StackTraceFrame {
                function: frame.current_function.name().to_owned(),
                offset: frame.index,
            })
            .collect();

        call_stack.push(StackTraceFrame {
            function: thread.current_frame.current_function.name().to_owned(),
            offset: thread.current_frame.index,
        });

        call_stack.reverse();

        Stack {
            entries: call_stack,
        }
    }
}

pub struct JsThread<'a> {
    pub(crate) stack: Vec<RuntimeValue<'a>>,
    call_stack: Vec<StackFrame<'a>>,
    catch_stack: Vec<CatchFrame>,
    current_frame: StackFrame<'a>,
    pub(crate) global_this: Realm<'a>,
    error: Option<ExecutionError<'a>>,
    cost_limit: Option<usize>,
    call_stack_limit: usize,
}

impl<'a> JsThread<'a> {
    pub(crate) fn current_context(&self) -> &JsContext<'a> {
        &self.current_frame.context
    }

    pub(crate) fn current_function(&self) -> &JsFunction {
        &self.current_frame.current_function
    }

    pub fn read_arg(&self, count: usize, index: usize) -> Option<RuntimeValue<'a>> {
        if index > count {
            None
        } else {
            self.stack.get(self.stack.len() - count).cloned()
        }
    }

    pub fn new(function: JsFunction, global_this: Realm<'a>) -> JsThread<'a> {
        let root = JsContext::root(&global_this);

        let context = JsContext::with_parent(
            root,
            global_this.global_this.clone(),
            &function,
            &global_this.wrappers,
        );

        JsThread {
            stack: Vec::with_capacity(1024),
            call_stack: Vec::new(),
            catch_stack: Vec::new(),
            current_frame: StackFrame {
                current_function: function,
                index: 0,
                stack_size: 0,
                context,
                is_new: false,
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

    pub(crate) fn push_stack(&mut self, value: impl Into<RuntimeValue<'a>>) {
        self.stack.push(value.into());
    }

    pub(crate) fn pop_stack<S: From<RuntimeValue<'a>>>(&mut self) -> S {
        self.stack.pop().unwrap().into()
    }

    pub(crate) fn return_value(&mut self, value: impl Into<RuntimeValue<'a>>) {
        let value = value.into();

        trace!(
            "return {:?} {:?} {:?}",
            value,
            self.current_frame,
            self.call_stack.last()
        );

        if let Some(parent) = self.call_stack.pop() {
            self.current_frame = parent;

            self.stack.truncate(self.current_frame.stack_size);
            self.stack.push(value);

            trace!("fixing catch stack {:?}", self.catch_stack);

            while let Some(CatchFrame { stack_frame, .. }) = self.catch_stack.last() {
                trace!(
                    "attempting to fix catch stack {} {}",
                    *stack_frame,
                    self.call_stack.len()
                );
                if self.call_stack.len() < *stack_frame {
                    trace!("Dropping catch");
                    self.catch_stack.pop();
                } else {
                    break;
                }
            }

            trace!("finished fixing catch stack {:?}", self.catch_stack);

            self.current_frame.index += 1;
        } else {
            self.current_frame.index = usize::MAX;
        }
    }

    pub(crate) fn is_new(&self) -> bool {
        self.current_frame.is_new
    }

    pub(crate) fn jump(&mut self, instruction_index: usize) {
        if let Some(limit) = self.cost_limit {
            if limit > 0 {
                self.cost_limit = Some(limit - 1)
            } else {
                return self.throw(InternalError::new_stackless("Hit execution limit"));
            }
        }

        self.current_frame.index = instruction_index;
    }

    pub(crate) fn throw(&mut self, error: impl Into<ExecutionError<'a>>) {
        let mut error = error.into();

        if let Some(limit) = self.cost_limit {
            if limit > 0 {
                self.cost_limit = Some(limit - 1)
            } else {
                error = InternalError::new_stackless("Hit execution limit").into();
            }
        }

        let stack: Stack = self.into();
        let error = error.fill_stack_trace(stack);

        if matches!(&error, ExecutionError::InternalError(_))
            || matches!(&error, ExecutionError::SyntaxError(_))
            || self.catch_stack.is_empty()
        {
            self.call_stack.truncate(1);
            if let Some(call) = self.call_stack.pop() {
                self.current_frame = call;
                self.stack.truncate(self.current_frame.stack_size);
            }

            self.current_frame.index = END_OF_FRAME;
            self.error = Some(error);
        } else if let ExecutionError::Thrown(error, stack) = error {
            let CatchFrame {
                instruction_index,
                stack_frame,
            } = self.catch_stack.last().unwrap();

            trace!(
                "Starting to unwind the stack\n{:?}\n{:?}\n{:?}",
                self.current_frame,
                self.catch_stack,
                self.call_stack
            );

            while self.call_stack.len() > *stack_frame {
                if let Some(call) = self.call_stack.pop() {
                    self.current_frame = call;

                    if self.current_frame.index == END_OF_FRAME {
                        // this is a native frame that we would need to unwind via
                        self.error = Some(ExecutionError::Thrown(error, stack));
                        return;
                    }

                    self.stack.truncate(self.current_frame.stack_size);
                }

                trace!(
                    "Unwinding\n{:?}\n{:?}\n{:?}",
                    self.current_frame,
                    self.catch_stack,
                    self.call_stack
                );
            }

            self.current_frame.index = *instruction_index;

            self.stack.push(error);
        } else {
            unreachable!("This should not be possible :(");
        }

        // match (error, self.catch_stack.pop()) {
        //     (ExecutionError::Thrown(error, ..), Some(CatchFrame { stack_frame, chunk })) => {
        //         self.call_stack.truncate(stack_frame + 1);
        //
        //         if let Some(call) = self.call_stack.pop() {
        //             self.current_frame = call;
        //             self.stack.truncate(self.current_frame.stack_size);
        //         }
        //
        //         self.current_frame.current_chunk_index = chunk;
        //         self.current_frame.index = 0;
        //         self.stack.push(error);
        //     }
        //     (error, ..) => {
        //         self.call_stack.truncate(1);
        //         if let Some(call) = self.call_stack.pop() {
        //             self.current_frame = call;
        //             self.stack.truncate(self.current_frame.stack_size);
        //         }
        //
        //         self.current_frame.index = usize::MAX;
        //         self.error = Some(error);
        //     }
        // }
    }

    pub(crate) fn catch(&mut self, chunk: usize) {
        self.catch_stack.push(CatchFrame {
            instruction_index: chunk,
            stack_frame: self.call_stack.len(),
        });
    }

    pub(crate) fn drop_catch(&mut self, chunk: usize) {
        let catch = self.catch_stack.pop().unwrap();

        assert_eq!(self.call_stack.len(), catch.stack_frame);
        assert_eq!(chunk, catch.instruction_index);
    }

    fn next_instruction(&self) -> Instruction {
        self.current_frame.current_function.instructions()[self.current_frame.index].clone()
    }

    pub(crate) fn call(
        &mut self,
        target: JsObject<'a>,
        CustomFunctionReference {
            function,
            parent_context: context,
        }: CustomFunctionReference<'a>,
        args: usize,
        new: bool,
        native: bool,
    ) {
        // println!("{} \n =====", function.name());
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
            context.clone(),
            target.clone(),
            &function,
            &self.global_this.wrappers,
        );

        let stack_length = self.stack.len();
        let arguments = (stack_length - args)..stack_length;

        for (write_to, read_from) in arguments.clone().enumerate().take(function.args_size()) {
            new_context.write(write_to + 1, self.stack[read_from].clone())
        }

        new_context.write(
            0,
            make_arguments(
                arguments.map(|_| self.stack.pop().unwrap()).collect(),
                &self.global_this,
            ),
        );

        self.current_frame.stack_size = self.stack.len();
        let new_frame = StackFrame {
            current_function: function,
            index: 0,
            context: new_context,
            stack_size: 0,
            is_new: new,
        };

        let existing_frame = std::mem::replace(&mut self.current_frame, new_frame);
        self.call_stack.push(existing_frame.clone());

        if native {
            let mut native_frame = existing_frame;
            native_frame.index = END_OF_FRAME;
            self.call_stack.push(native_frame);
        }
    }

    pub fn run(&mut self) -> JsResult<'a, RuntimeValue<'a>> {
        while self.current_frame.index < self.current_frame.current_function.instructions().len() {
            let frame_index = self.current_frame.index;

            trace!(
                "{:3} {:?}",
                frame_index,
                DebuggableInstruction::new(&self.next_instruction(), self)
            );

            // trace!("{:?} \n result: {:?}", &chunk.instructions[index], frame);

            self.next_instruction().execute(self);
        }

        if let Some(err) = std::mem::replace(&mut self.error, None) {
            Err(err)
        } else {
            Ok(self.stack.pop().unwrap_or_default())
        }
    }

    pub(crate) fn call_from_native(
        &mut self,
        target: JsObject<'a>,
        function_reference: impl Into<FunctionReference<'a>>,
        args: usize,
        new: bool,
    ) -> JsResult<'a, RuntimeValue<'a>> {
        match function_reference.into() {
            FunctionReference::BuiltIn(builtin) => {
                let result = builtin.apply_return(args, self, Some(target))?;

                Ok(result.unwrap_or_default())
            }
            FunctionReference::Custom(custom) => {
                self.call(target, custom, args, new, true);

                let result = self.run();

                println!("Returning from native call {:?} {:?}", result, self.stack);

                self.current_frame = self.call_stack.pop().unwrap();

                result
            }
        }
    }
}

struct DebuggableInstruction<'a, 'b> {
    instruction: &'a Instruction,
    thread: &'a mut JsThread<'b>,
}

impl<'a, 'b> DebuggableInstruction<'a, 'b> {
    fn new(instruction: &'a Instruction, thread: &'a mut JsThread<'b>) -> Self {
        DebuggableInstruction {
            instruction,
            thread,
        }
    }
}

impl<'a, 'b> Debug for DebuggableInstruction<'a, 'b> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.instruction {
            Instruction::GetNamed { name } => {
                let atom = self.thread.current_frame.current_function.get_atom(*name);

                f.debug_struct("GetNamed")
                    .field("name", &atom.to_string())
                    .finish()
            }
            Instruction::GetLocal { local } => f
                .debug_struct("GetLocal")
                .field("local", local)
                .field("value", &self.thread.current_context().read(*local))
                .finish(),
            Instruction::SetNamed { name } => {
                let atom = self.thread.current_frame.current_function.get_atom(*name);

                f.debug_struct("GetNamed")
                    .field("name", &atom.to_string())
                    .finish()
            }
            instr => Instruction::fmt(instr, f),
        }
    }
}