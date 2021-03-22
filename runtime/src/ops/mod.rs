#[macro_use]
mod macros;
mod bitwise;
mod comparison;
mod control_flow;
mod math;
mod memory;

use crate::ops::bitwise::{left_shift, right_shift, right_shift_unsigned};
use crate::ops::comparison::{
    equal_to, greater_than, greater_than_equal, in_operator, instance_of, less_than,
    less_than_equal, logical_and, logical_not, logical_or, not_equal_to, not_strict_equal_to,
    strict_equal_to, type_of,
};
use crate::ops::control_flow::{
    call, call_new, catch, compare_jump, drop_catch, jump, return_constant, return_value,
    throw_value,
};
use crate::ops::math::{add, divide, exponential, increment, modulus, multiply, negate, subtract};
use crate::ops::memory::{
    delete, duplicate, get, get_capture, get_class, get_function, get_local, get_named,
    load_constant, load_environmental, resolve, set, set_capture, set_local, set_named,
};
use crate::vm::JsThread;
use instruction_set::Instruction;

pub(crate) trait Operand {
    fn execute<'a>(&self, thread: &mut JsThread<'a>);
}

impl Operand for Instruction {
    fn execute<'a>(&self, thread: &mut JsThread<'a>) {
        match self {
            Instruction::Truncate => {
                thread.step();
            }
            Instruction::Add { times } => add(thread, *times),
            Instruction::Subtract => subtract(thread),
            Instruction::Divide => divide(thread),
            Instruction::Modulo => modulus(thread),
            Instruction::Exponential => exponential(thread),
            Instruction::Multiply => multiply(thread),
            Instruction::LoadConstant { constant } => load_constant(thread, constant),
            Instruction::LoadEnvironmental { environmental } => {
                load_environmental(thread, environmental)
            }
            Instruction::Return => return_value(thread),
            Instruction::ReturnConstant { constant } => return_constant(thread, constant),
            Instruction::Call { args_count } => call(thread, *args_count),
            Instruction::CallNew { args_count } => call_new(thread, *args_count),
            Instruction::Jump { to } => jump(thread, *to),
            Instruction::CompareJump { if_true, if_false } => {
                compare_jump(thread, *if_true, *if_false)
            }

            Instruction::SetLocal { local } => set_local(thread, *local),
            Instruction::SetCapture { frame, local } => set_capture(thread, *frame, *local),
            Instruction::SetNamed { name } => set_named(thread, *name),
            Instruction::Set => set(thread),

            Instruction::GetLocal { local } => get_local(thread, *local),
            Instruction::GetCapture { frame, local } => get_capture(thread, *frame, *local),
            Instruction::GetNamed { name } => get_named(thread, *name),
            Instruction::GetFunction { function } => get_function(thread, *function),
            Instruction::GetClass { class, extends } => get_class(thread, *class, *extends),
            Instruction::Get => get(thread),

            Instruction::Delete => delete(thread),
            Instruction::DeleteNamed { .. } | Instruction::DeleteLocal { .. } => {
                todo!("Not yet implemented")
            }

            Instruction::StrictEqualTo => strict_equal_to(thread),
            Instruction::NotStrictEqualTo => not_strict_equal_to(thread),
            Instruction::EqualTo => equal_to(thread),
            Instruction::NotEqualTo => not_equal_to(thread),
            Instruction::TypeOf => type_of(thread),
            Instruction::LogicalNot => logical_not(thread),
            Instruction::Neg => negate(thread),
            Instruction::LessThan => less_than(thread),
            Instruction::LessThanEqual => less_than_equal(thread),
            Instruction::GreaterThan => greater_than(thread),
            Instruction::GreaterThanEqual => greater_than_equal(thread),
            Instruction::LogicalOr { left, right } => logical_or(thread, *left, *right),
            Instruction::LogicalAnd { left, right } => logical_and(thread, *left, *right),
            Instruction::ThrowValue => throw_value(thread),
            Instruction::RightShiftUnsigned => right_shift_unsigned(thread),
            Instruction::RightShift => right_shift(thread),
            Instruction::LeftShift => left_shift(thread),
            Instruction::InstanceOf => instance_of(thread),
            Instruction::Increment { by } => increment(thread, *by),
            Instruction::Catch { chunk } => catch(thread, *chunk),
            Instruction::DropCatch { chunk } => drop_catch(thread, *chunk),
            Instruction::Duplicate => duplicate(thread),
            Instruction::Resolve => resolve(thread),
            Instruction::In => in_operator(thread),
        }
    }
}
