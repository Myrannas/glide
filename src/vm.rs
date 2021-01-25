use crate::ops::{Context, Instruction, Next, RuntimeFrame};
use crate::value::RuntimeValue;
use nom::lib::std::fmt::{Debug, Formatter};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Module {
    pub init: Function,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Module{}")?;
        Ok(())
    }
}

impl Module {
    pub(crate) fn load(&self) -> Result<(), ExecutionError> {
        self.init.execute(None)?;
        Ok(())
    }
}

pub struct Function {
    pub(crate) instructions: Vec<Instruction>,
    pub stack_size: usize,
    pub local_size: usize,
    pub functions: Vec<Function>,
    pub name: String,
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}()", self.name))
    }
}

#[derive(Debug)]
pub enum ExecutionError {}

impl Function {
    pub(crate) fn execute<'a>(
        &'a self,
        parent: Option<Rc<RefCell<Context<'a>>>>,
    ) -> Result<RuntimeValue<'a>, ExecutionError> {
        let mut frame = RuntimeFrame::<'a> {
            context: Context::with_parent(parent, self.local_size),
            stack: Vec::with_capacity(self.stack_size),
            function: &self,
        };

        for Instruction { instr, constant } in self.instructions.iter() {
            let result = instr(constant, &mut frame);

            println!("result: {:?}", result);

            match result {
                Next::Step => {}
                Next::Return(value) => return Ok(value),
                Next::Call(RuntimeValue::Function(function, context)) => {
                    let result = function.execute(Some(context))?;

                    frame.stack.push(result);
                }
                test => panic!("Unhandled step type {:?}", test),
            }
        }

        panic!("Block finished abnormally")

        // loop {
        //     for instr in self.instructions[chunk].iter() {
        //         match instr {
        //             Instruction::Add => {
        //                 let left = vec.pop().unwrap();
        //                 let right = vec.pop().unwrap();
        //                 vec.push(left + right)
        //             },
        //             Instruction::Sub => {
        //                 let left = vec.pop().unwrap();
        //                 let right = vec.pop().unwrap();
        //                 vec.push(left - right)
        //             },
        //             Instruction::Push(value) => {
        //                 vec.push(value.clone());
        //             }
        //             Instruction::Jmp{ chunk: c } => {
        //                 chunk = *c;
        //                 break;
        //             }
        //             Instruction::Return => {
        //                 return vec.pop().unwrap();
        //             },
        //             Instruction::BranchTruthy { chunk: c } => {
        //                 let v = vec.pop().unwrap();
        //œ
        //                 if v == Value::True {
        //                     chunk = *c;
        //                     break;
        //                 }
        //             }
        //             _ => panic!("Unsupported instruction")
        //         }
        //     }
        // }
    }
}

// fn add(_: Value, mut stack: Vec<Value>) -> Vec<Value> {
//     let l_ref = stack.pop().expect("Stack should have at two values to use add operator");
//     // let l_value = l_ref.resolve();
//     let l_prim = l_ref;
//
//     let r_ref = stack.pop().expect("Stack should have at two values to use add operator");
//     // let r_value = r_ref.resolve();
//     let r_prim = r_ref;
//
//     // no strings yet
//
//     let l_num: f64 = l_prim.into();
//     let r_num: f64 = r_prim.into();
//
//     stack.push(Value::Float(l_num + r_num));
//
//     stack
// }
//
// fn load(value: Value, mut stack: Vec<Value>) -> Vec<Value> {
//     stack.push(value.clone());
//
//     stack
// }
