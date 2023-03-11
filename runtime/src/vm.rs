use crate::context::JsContext;
use crate::debugging::{DebugRepresentation, DebugWithRealm, X};
use crate::ops::Operand;
use crate::primordials::{Realm, RuntimeHelpers};
use crate::result::{InternalError, JsResult, Stack, StackTraceFrame};
use crate::values::function::{CustomFunctionReference, FunctionReference};
use crate::values::nan::Value;
use crate::values::value::make_arguments;
use crate::{ExecutionError, JsFunction};
use instruction_set::{Constant, Instruction};
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
        let strings = &thread.realm.strings;

        let mut call_stack: Vec<StackTraceFrame> = thread
            .call_stack
            .iter()
            .map(|frame| StackTraceFrame {
                function: strings
                    .get(frame.current_function.name())
                    .as_ref()
                    .to_owned(),
                offset: frame.index,
            })
            .collect();

        call_stack.push(StackTraceFrame {
            function: strings
                .get(thread.current_frame.current_function.name())
                .as_ref()
                .to_owned(),
            offset: thread.current_frame.index,
        });

        call_stack.reverse();

        Stack {
            entries: call_stack,
        }
    }
}

pub struct JsThread<'a> {
    pub(crate) stack: Vec<Value<'a>>,
    call_stack: Vec<StackFrame<'a>>,
    catch_stack: Vec<CatchFrame>,
    current_frame: StackFrame<'a>,
    pub(crate) realm: Realm<'a>,
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

    #[must_use]
    pub fn read_arg(&self, count: usize, index: usize) -> Option<Value<'a>> {
        if index > count {
            None
        } else {
            self.stack.get(self.stack.len() - count + index).cloned()
        }
    }

    #[must_use]
    pub fn new(function: JsFunction, mut global_this: Realm<'a>) -> JsThread<'a> {
        let root = JsContext::root(&global_this);

        let this = global_this.global_this.clone().into();
        let context = JsContext::with_parent(&mut global_this, root, this, &function);

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
            realm: global_this,
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

    pub(crate) fn push_stack(&mut self, value: impl Into<Value<'a>>) {
        self.stack.push(value.into());
    }

    pub(crate) fn pop_stack<S: From<Value<'a>>>(&mut self) -> S {
        self.stack.pop().unwrap().into()
    }

    pub(crate) fn return_value(&mut self, value: impl Into<Value<'a>>) {
        let value = value.into();

        trace!(
            "return {:?} {:?} {:?}",
            self.debug_value(&value),
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
        #[cfg(feature = "cost_limits")]
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

        if let ExecutionError::TypeError(msg) = &error {
            error = ExecutionError::Thrown(self.new_type_error(msg), None);
        }

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
        target: Value<'a>,
        CustomFunctionReference {
            function,
            parent_context: context,
        }: CustomFunctionReference<'a>,
        args: usize,
        new: bool,
        native: bool,
    ) {
        let args_debug: Vec<Value<'a>> = self.stack.iter().rev().take(args).cloned().collect();
        trace!(
            "{}({:?}) \n =====",
            self.realm.get_string(function.name()),
            X::from(&args_debug, &self.realm)
        );
        if self.call_stack.len() > self.call_stack_limit {
            return self.throw(InternalError::new_stackless("Stack overflow"));
        }

        #[cfg(feature = "cost_limits")]
        if let Some(limit) = self.cost_limit {
            if limit > 0 {
                self.cost_limit = Some(limit - 1)
            } else {
                return self.throw(InternalError::new_stackless("Hit execution limit"));
            }
        }

        let new_context =
            JsContext::with_parent(&mut self.realm, context.clone(), target, &function);

        let stack_length = self.stack.len();
        let arguments = (stack_length - args)..stack_length;

        for (write_to, read_from) in arguments.clone().enumerate().take(function.args_size()) {
            new_context.write(write_to + 1, self.stack[read_from])
        }

        new_context.write(
            0,
            make_arguments(
                arguments
                    .map(|_| self.stack.pop().unwrap_or_default())
                    .collect(),
                self,
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

    pub fn run(&mut self) -> JsResult<'a, Value<'a>> {
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

        std::mem::replace(&mut self.error, None)
            .map_or_else(|| Ok(self.stack.pop().unwrap_or_default()), Err)
    }

    pub fn call_from_native(
        &mut self,
        target: Value<'a>,
        function_reference: impl Into<FunctionReference<'a>>,
        args: usize,
        new: bool,
    ) -> JsResult<'a, Value<'a>> {
        match function_reference.into() {
            FunctionReference::BuiltIn(builtin) => {
                let result = builtin.apply_return(args, self, target)?;

                Ok(result.unwrap_or_default())
            }
            FunctionReference::Custom(custom) => {
                self.call(target, custom, args, new, true);

                let result = self.run();

                self.current_frame = self.call_stack.pop().unwrap();

                result
            }
        }
    }

    #[must_use]
    pub fn debug<'b, 'c>(&self, value: &impl DebugRepresentation<'a>) -> String {
        format!("{:?}", X::from(value, self.get_realm()))
    }

    #[inline]
    pub fn get_realm_mut(&mut self) -> &mut Realm<'a> {
        &mut self.realm
    }

    #[inline]
    #[must_use]
    pub fn get_realm(&self) -> &Realm<'a> {
        &self.realm
    }

    #[must_use]
    pub fn finalize(self) -> Realm<'a> {
        self.realm
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
                let str = self.thread.realm.strings.get(atom).as_ref();

                f.debug_struct("GetNamed").field("name", &str).finish()
            }
            Instruction::GetLocal { local } => {
                let locals = &self.thread.current_frame.current_function.locals()[*local];

                f.debug_struct("GetLocal")
                    .field("local", &locals.name)
                    .field("local_id", local)
                    .finish()
            }

            Instruction::SetLocal { local } => {
                let locals = &self.thread.current_frame.current_function.locals()[*local];
                let value1 = self.thread.stack.last().cloned().unwrap_or_default();
                let last = X::from(&value1, self.thread.get_realm());

                f.debug_struct("SetLocal")
                    .field("local", &locals.name)
                    .field("local_id", local)
                    .field("value", &last)
                    .finish()
            }

            Instruction::SetNamed { name } => {
                let atom = self.thread.current_frame.current_function.get_atom(*name);
                let str = self.thread.realm.strings.get(atom).as_ref();

                let value = self.thread.stack.last().cloned().unwrap_or_default();
                let last = X::from(&value, self.thread.get_realm());
                f.debug_struct("SetLocal")
                    .field("field", &str)
                    .field("value", &last)
                    .finish()
            }

            Instruction::LoadConstant {
                constant: Constant::Atom(atom),
            } => {
                let atom = self.thread.current_frame.current_function.get_atom(*atom);
                let str = self.thread.realm.strings.get(atom).as_ref();

                f.debug_struct("LoadConstant").field("value", &str).finish()
            }

            instr => Instruction::fmt(instr, f),
        }
    }
}
