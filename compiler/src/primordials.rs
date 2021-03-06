use crate::compiler::compile_eval;
use crate::object::JsObject;
use crate::result::JsResult;
use crate::value::StaticValue::Object;
use crate::value::{BuiltIn, BuiltinFn, CustomFunctionReference, FunctionReference, RuntimeValue};
use crate::vm::JsThread;
use crate::{parse_input, InternalError};
use std::rc::Rc;

fn eval<'a>(
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
                    return Err(err.into());
                }
            };
            let function = match compile_eval(frame.current_function(), &input, code) {
                Ok(code) => code,
                Err(err) => {
                    return Err(err.into());
                }
            };

            let this = frame.current_context().this().clone();
            let context = frame.current_context().clone();

            frame.call(
                this,
                CustomFunctionReference { function, context },
                0,
                false,
                false,
            );

            Ok(None)
        }
        _ => Ok(Some(RuntimeValue::Undefined)),
    }
}

fn string_length<'a>(
    _args: &[Option<RuntimeValue<'a>>],
    _thread: &mut JsThread<'a>,
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

impl<'a> Helpers<'a> for JsObject<'a> {
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
                op: |_, _thread, _, context| Ok(Some(context.unwrap().clone())),
                desired_args: 0,
            })),
            None,
        )
    }
}

#[derive(Clone)]
pub(crate) struct Errors<'a> {
    reference_error: JsObject<'a>,
    syntax_error: JsObject<'a>,
    type_error: JsObject<'a>,
    error: JsObject<'a>,
}

#[derive(Clone)]
pub(crate) struct Primitives<'a> {
    string: JsObject<'a>,
}

#[derive(Clone)]
pub struct GlobalThis<'a> {
    pub(crate) global_this: JsObject<'a>,
    pub(crate) errors: Errors<'a>,
    pub(crate) wrappers: Primitives<'a>,
}

impl<'a> Default for GlobalThis<'a> {
    fn default() -> Self {
        GlobalThis::new()
    }
}

impl<'a> GlobalThis<'a> {
    pub fn new() -> GlobalThis<'a> {
        let global_this = JsObject::new();

        let math = JsObject::new();
        math.define_readonly_builtin(
            "pow",
            |_args, _thread, _target, _context| Ok(Some(RuntimeValue::Undefined)),
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
                JsObject::new(),
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
    fn init(global_this: &JsObject<'a>) -> Primitives<'a> {
        let string_prototype = JsObject::new();

        string_prototype.define_readonly_value(
            "constructor",
            BuiltIn {
                op: |args, thread, _, ctx| {
                    let value = args
                        .get(0)
                        .cloned()
                        .unwrap()
                        .unwrap_or(RuntimeValue::Undefined);
                    let str = value.to_string(thread)?;
                    let prototype = ctx.unwrap().as_object()?.clone();

                    Ok(Some(RuntimeValue::String(str, prototype)))
                },
                desired_args: 1,
                context: Some(Box::new(RuntimeValue::Object(string_prototype.clone()))),
            },
        );
        string_prototype.define_readonly_builtin("length", string_length, 0);

        let string_function = BuiltIn {
            op: |args, thread, _, ctx| {
                let value = args
                    .get(0)
                    .cloned()
                    .unwrap()
                    .unwrap_or(RuntimeValue::Undefined);
                let str = value.to_string(thread)?;
                let prototype = ctx.unwrap().as_object()?.clone();

                Ok(Some(RuntimeValue::String(str, prototype)))
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
    #[allow(dead_code)]
    pub(crate) fn new_reference_error(&self, message: impl Into<String>) -> RuntimeValue<'a> {
        self.new_error(&self.reference_error, message.into()).into()
    }

    #[allow(dead_code)]
    pub(crate) fn new_syntax_error(&self, message: impl Into<String>) -> RuntimeValue<'a> {
        self.new_error(&self.syntax_error, message.into()).into()
    }

    pub(crate) fn new_type_error(&self, message: impl Into<String>) -> RuntimeValue<'a> {
        self.new_error(&self.type_error, message.into()).into()
    }

    fn new_error(&self, prototype: &JsObject<'a>, message: impl Into<String>) -> JsObject<'a> {
        let error = JsObject::new().with_prototype(prototype.clone());

        error.set(
            Rc::new("message".into()),
            RuntimeValue::String(Rc::new(message.into()), JsObject::new()),
        );

        error
    }

    fn init(global_this: &JsObject<'a>) -> Errors<'a> {
        let error = JsObject::new();

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

        error.define_readonly_value(
            "toString",
            BuiltIn {
                context: None,
                desired_args: 1,
                op: |_args, thread, target, _| {
                    let obj = target.as_object()?;

                    let message = obj.get(Rc::new("message".into()), thread, target)?;

                    Ok(Some(RuntimeValue::String(
                        message.to_string(thread)?,
                        JsObject::new(),
                    )))
                },
            },
        );

        let reference_error = JsObject::new().with_prototype(error.clone());
        let syntax_error = JsObject::new().with_prototype(error.clone());
        let type_error = JsObject::new().with_prototype(error.clone());

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
