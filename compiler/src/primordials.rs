use crate::compiler::compile_eval;
use crate::object::{JsObject, Object, ObjectMethods};
use crate::ops::{ControlFlow, RuntimeFrame};
use crate::parser::ast::Statement::Continue;
use crate::result::{ExecutionError, JsResult};
use crate::value::{BuiltIn, BuiltinFn, CustomFunctionReference, FunctionReference, RuntimeValue};
use crate::vm::JsThread;
use crate::{parse_input, CompilerOptions, InternalError};
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

fn object_constructor<'a, 'b>(
    args: &[Option<RuntimeValue<'a>>],
    thread: &mut JsThread<'a>,
    _: &RuntimeValue<'a>,
    _context: Option<&RuntimeValue<'a>>,
) -> JsResult<'a, Option<RuntimeValue<'a>>> {
    Ok(Some(RuntimeValue::Object(Object::create())))
}

fn array_constructor<'a, 'b>(
    args: &[Option<RuntimeValue<'a>>],
    thread: &mut JsThread<'a>,
    _: &RuntimeValue<'a>,
    _context: Option<&RuntimeValue<'a>>,
) -> JsResult<'a, Option<RuntimeValue<'a>>> {
    Ok(Some(RuntimeValue::Object(Rc::new(RefCell::new(
        JsObject {
            prototype: None,
            indexed_properties: Some(vec![]),
            properties: None,
            name: None,
        },
    )))))
}

fn boolean_constructor<'a, 'b>(
    args: &[Option<RuntimeValue<'a>>],
    thread: &mut JsThread<'a>,
    _: &RuntimeValue<'a>,
    _context: Option<&RuntimeValue<'a>>,
) -> JsResult<'a, Option<RuntimeValue<'a>>> {
    Ok(Some(Object::create().into()))
}

fn eval<'a, 'b>(
    args: &[Option<RuntimeValue<'a>>],
    frame: &mut JsThread<'a>,
    _value: &RuntimeValue<'a>,
    _context: Option<&RuntimeValue<'a>>,
) -> JsResult<'a, Option<RuntimeValue<'a>>> {
    match &args[0] {
        Some(RuntimeValue::String(str, ..)) => {
            let input = str.as_ref().to_owned();

            let code = match parse_input(&input) {
                Ok(code) => code,
                Err(err) => {
                    frame.throw(err);
                    return Ok(None);
                }
            };
            let function = match compile_eval(frame.current_function(), &input, code) {
                Ok(code) => code,
                Err(err) => {
                    frame.throw(err);
                    return Ok(None);
                }
            };

            let this = frame.current_context().this().clone();
            let context = frame.current_context().clone();

            frame.call(
                this,
                CustomFunctionReference { function, context },
                0,
                false,
            );

            Ok(None)
        }
        _ => Ok(Some(RuntimeValue::Undefined)),
    }
}

fn string_length<'a, 'b>(
    args: &[Option<RuntimeValue<'a>>],
    thread: &mut JsThread<'a>,
    value: &RuntimeValue<'a>,
    _context: Option<&RuntimeValue<'a>>,
) -> JsResult<'a, Option<RuntimeValue<'a>>> {
    match value {
        RuntimeValue::String(str, ..) => Ok(Some(RuntimeValue::Float(str.as_ref().len() as f64))),
        _ => InternalError::new_stackless("Attempted to call string length on non-string").into(),
    }
}

pub(crate) fn make_type_error<'a, 'b>(frame: &mut JsThread<'a>) -> JsResult<'a> {
    Ok(RuntimeValue::Object(Rc::new(RefCell::new(JsObject {
        prototype: Some(
            frame
                .global_this
                .clone()
                .get(Rc::new("TypeError".to_owned()), frame, &RuntimeValue::Null)?
                .as_object()?
                .clone(),
        ),
        indexed_properties: Some(vec![]),
        properties: None,
        name: None,
    }))))
}

trait Helpers<'a> {
    fn define_readonly_builtin<S: Into<String>>(
        &self,
        key: S,
        getter: BuiltinFn<'a>,
        desired_args: usize,
    );
    fn define_readonly_value<S: Into<String>, V: Into<RuntimeValue<'a>>>(&self, key: S, value: V);
}

impl<'a> Helpers<'a> for Object<'a> {
    fn define_readonly_builtin<S: Into<String>>(
        &self,
        key: S,
        getter: BuiltinFn<'a>,
        desired_args: usize,
    ) {
        self.define_property(
            Rc::new(key.into()),
            Some(FunctionReference::BuiltIn(BuiltIn {
                op: getter,
                context: None,
                desired_args,
            })),
            None,
        )
    }

    fn define_readonly_value<S: Into<String>, V: Into<RuntimeValue<'a>>>(&self, key: S, value: V) {
        self.define_property(
            Rc::new(key.into()),
            Some(FunctionReference::BuiltIn(BuiltIn {
                context: Some(Box::new(value.into())),
                op: |_, thread, _, context| Ok(Some(context.unwrap().clone())),
                desired_args: 0,
            })),
            None,
        )
    }
}

// trait FrameHelpers<'a> {
//     fn with_args(
//         &mut self,
//         expect: usize,
//         op: impl Fn(&RuntimeFrame, &[Option<&RuntimeValue<'a>>]) -> JsResult<'a>,
//     ) -> JsResult<'a>;
// }
//
// impl<'a, 'b, 'c> FrameHelpers<'a> for RuntimeFrame<'a, 'b, 'c> {
//     #[inline]
//     fn with_args(
//         &mut self,
//         expect: usize,
//         op: impl Fn(&RuntimeFrame, &[Option<&RuntimeValue<'a>>]) -> JsResult<'a>,
//     ) -> JsResult<'a> {
//         let args_count = self.stack.len() - 1;
//         let required_arg_count = expect.min(8);
//         let mut args: [Option<&RuntimeValue<'a>>; 8] = Default::default();
//
//         for i in 0..required_arg_count {
//             if i < args_count {
//                 args[i] = self.stack.get(self.stack.len() - i - 1);
//             } else {
//                 args[i] = None;
//             }
//         }
//
//         let result = op(self, &args[0..required_arg_count]);
//
//         for _ in 0..args_count {
//             self.stack.pop();
//         }
//
//         result
//     }
// }

pub fn create_global<'a>() -> Object<'a> {
    let globals = Object::create_named("globalThis", None);

    globals.define_readonly_value("NaN", f64::NAN);

    let string_prototype = Object::create();
    let string_constructor = RuntimeValue::Function(
        FunctionReference::BuiltIn(BuiltIn {
            op: |args, thread, _, prototype| {
                let prototype: Object<'a> = prototype.unwrap().clone().try_into()?;
                let str_value: String = args[0]
                    .clone()
                    .map(|value| value.into())
                    .unwrap_or_else(|| "".to_owned());
                let str_obj = Object::create_named("String", Some(prototype));

                str_obj.define_readonly_value(
                    "___internal___string",
                    RuntimeValue::String(Rc::new(str_value), str_obj.clone()),
                );

                Ok(Some(RuntimeValue::Object(str_obj)))
            },
            context: Some(Box::new(string_prototype.clone().into())),
            desired_args: 1,
        }),
        string_prototype.clone(),
    );

    string_prototype.define_readonly_value("constructor", string_constructor.clone());
    string_prototype.define_readonly_builtin("length", string_length, 0);

    globals.define_readonly_value("String", string_constructor);
    globals.define_readonly_builtin("Object", object_constructor, 0);
    globals.define_readonly_builtin("Boolean", boolean_constructor, 0);
    globals.define_readonly_builtin("Array", array_constructor, 0);

    let math = Object::create();
    math.define_readonly_builtin(
        "pow",
        |_args, thread, _target, _context| Ok(Some(RuntimeValue::Undefined)),
        0,
    );

    globals.define_readonly_value("Math", math);
    globals.define_readonly_value(
        "eval",
        RuntimeValue::Function(
            FunctionReference::BuiltIn(BuiltIn {
                context: None,
                desired_args: 1,
                op: eval,
            }),
            Object::create(),
        ),
    );
    globals.define_readonly_value("undefined", RuntimeValue::Undefined);

    globals
}
