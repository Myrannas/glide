use crate::compiler::compile_eval;
use crate::object::{JsObject, Object, ObjectMethods};
use crate::ops::RuntimeFrame;
use crate::parser::ast::Statement::Continue;
use crate::result::{ExecutionError, JsResult};
use crate::value::{BuiltIn, BuiltinFn, CustomFunctionReference, FunctionReference, RuntimeValue};
use crate::vm::JsThread;
use crate::{parse_input, CompilerOptions, InternalError};
use std::cell::RefCell;
use std::convert::TryInto;
use std::fmt::Error;
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

    globals.define_readonly_value("String", string_constructor);
    globals.define_readonly_builtin("Object", object_constructor, 0);
    globals.define_readonly_builtin("Boolean", boolean_constructor, 0);
    globals.define_readonly_builtin("Array", array_constructor, 0);

    globals
}

#[derive(Clone)]
pub(crate) struct Errors<'a> {
    reference_error: Object<'a>,
    syntax_error: Object<'a>,
    type_error: Object<'a>,
    error: Object<'a>,
}

#[derive(Clone)]
pub(crate) struct Primitives<'a> {
    string: Object<'a>,
}

#[derive(Clone)]
pub struct GlobalThis<'a> {
    pub(crate) global_this: Object<'a>,
    pub(crate) errors: Errors<'a>,
    pub(crate) wrappers: Primitives<'a>,
}

impl<'a> GlobalThis<'a> {
    pub fn new() -> GlobalThis<'a> {
        let global_this = Object::create();

        let math = Object::create();
        math.define_readonly_builtin(
            "pow",
            |_args, thread, _target, _context| Ok(Some(RuntimeValue::Undefined)),
            0,
        );

        global_this.define_readonly_value("Math", math);
        global_this.define_readonly_value(
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
        global_this.define_readonly_value("undefined", RuntimeValue::Undefined);
        global_this.define_readonly_value("NaN", f64::NAN);

        let primitives = Primitives::init(&global_this);
        let errors = Errors::init(&global_this);

        GlobalThis {
            global_this,
            wrappers: primitives,
            errors,
        }
    }
}

impl<'a> Primitives<'a> {
    fn init(global_this: &Object<'a>) -> Primitives<'a> {
        let string_prototype = Object::create();

        string_prototype.define_readonly_value(
            "constructor",
            BuiltIn {
                op: |args, _, _, ctx| {
                    let value = args
                        .get(0)
                        .cloned()
                        .unwrap()
                        .unwrap_or(RuntimeValue::Undefined);
                    let str: String = value.into();
                    let prototype = ctx.unwrap().as_object()?.clone();

                    Ok(Some(RuntimeValue::String(Rc::new(str), prototype)))
                },
                desired_args: 1,
                context: Some(Box::new(RuntimeValue::Object(string_prototype.clone()))),
            },
        );
        string_prototype.define_readonly_builtin("length", string_length, 0);

        let string_function = BuiltIn {
            op: |args, _, _, ctx| {
                let value = args
                    .get(0)
                    .cloned()
                    .unwrap()
                    .unwrap_or(RuntimeValue::Undefined);
                let str: String = value.into();
                let prototype = ctx.unwrap().as_object()?.clone();

                Ok(Some(RuntimeValue::String(Rc::new(str), prototype)))
            },
            desired_args: 1,
            context: Some(Box::new(RuntimeValue::Object(string_prototype.clone()))),
        };

        global_this.define_readonly_value(
            "String",
            RuntimeValue::Function(string_function.into(), string_prototype.clone()),
        );

        Primitives {
            string: string_prototype,
        }
    }
}

impl<'a> Errors<'a> {
    pub(crate) fn new_reference_error(&self, message: impl Into<String>) -> RuntimeValue<'a> {
        self.new_error(&self.reference_error, message.into()).into()
    }

    pub(crate) fn new_syntax_error(&self, message: impl Into<String>) -> RuntimeValue<'a> {
        self.new_error(&self.syntax_error, message.into()).into()
    }

    pub(crate) fn new_type_error(&self, message: impl Into<String>) -> RuntimeValue<'a> {
        self.new_error(&self.type_error, message.into()).into()
    }

    fn new_error(&self, prototype: &Object<'a>, message: impl Into<String>) -> Object<'a> {
        let error = Object::with_prototype(prototype.clone());

        error.set(
            Rc::new("message".into()),
            RuntimeValue::String(Rc::new(message.into()), Object::create()),
        );

        error
    }

    fn init(global_this: &Object<'a>) -> Errors<'a> {
        let error = Object::create();

        error.define_readonly_value(
            "constructor",
            BuiltIn {
                context: None,
                desired_args: 1,
                op: |args, _, target, _| {
                    let obj = target.as_object()?;

                    let message = args
                        .get(0)
                        .cloned()
                        .unwrap()
                        .unwrap_or(RuntimeValue::Undefined);

                    obj.set(Rc::new("message".into()), message);

                    Ok(None)
                },
            },
        );

        let reference_error = Object::with_prototype(error.clone());
        let syntax_error = Object::with_prototype(error.clone());
        let type_error = Object::with_prototype(error.clone());

        global_this.define_readonly_value("ReferenceError", reference_error.clone());
        global_this.define_readonly_value("SyntaxError", syntax_error.clone());
        global_this.define_readonly_value("TypeError", type_error.clone());
        global_this.define_readonly_value("Error", error.clone());

        Errors {
            syntax_error,
            reference_error,
            type_error,
            error,
        }
    }
}
