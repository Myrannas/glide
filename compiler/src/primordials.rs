use super::builtins::errors;
use super::builtins::prototype::Prototype;
use crate::compiler::compile_eval;
use crate::object::JsObject;
use crate::parse_input;
use crate::result::JsResult;
use crate::value::{BuiltIn, BuiltinFn, CustomFunctionReference, FunctionReference, RuntimeValue};
use crate::vm::JsThread;
use std::rc::Rc;

fn eval<'a>(
    args: &[Option<RuntimeValue<'a>>],
    frame: &mut JsThread<'a>,
    _value: &JsObject<'a>,
    _context: Option<&RuntimeValue<'a>>,
) -> JsResult<'a, Option<RuntimeValue<'a>>> {
    match &args[0] {
        Some(RuntimeValue::String(str)) => {
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
    function: JsObject<'a>,
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

        let primitives = Primitives::init(&global_this);
        let errors = Errors::init(&global_this);

        global_this.define_value("Math", super::builtins::math::JsMath::bind(None));

        global_this.define_readonly_value(
            "eval",
            primitives.wrap_function(BuiltIn {
                context: None,
                desired_args: 1,
                op: eval,
            }),
        );
        global_this.define_readonly_value("undefined", RuntimeValue::Undefined);
        global_this.define_readonly_value("NaN", f64::NAN);

        GlobalThis {
            global_this,
            wrappers: primitives,
            errors,
        }
    }
}

impl<'a> Primitives<'a> {
    fn init(global_this: &JsObject<'a>) -> Primitives<'a> {
        let string_prototype: JsObject<'a> = super::builtins::string::JsString::bind(None);
        let number_prototype: JsObject<'a> = super::builtins::number::JsNumber::bind(None);
        let function_prototype = JsObject::new();

        let primitives = Primitives {
            string: string_prototype.clone(),
            function: function_prototype.clone(),
        };

        global_this.define_value("String", string_prototype);
        global_this.define_value("Number", number_prototype.clone());
        global_this.define_value(
            "parseInt",
            number_prototype.read_simple_property("parseInt"),
        );

        primitives
    }

    pub(crate) fn wrap_string(&self, string: Rc<String>) -> JsObject<'a> {
        JsObject::new()
            .wrapping(RuntimeValue::String(string))
            .with_prototype(self.string.clone())
    }

    pub(crate) fn wrap_function(&self, function: impl Into<FunctionReference<'a>>) -> JsObject<'a> {
        JsObject::new()
            .callable(function)
            .with_prototype(self.function.clone())
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
            RuntimeValue::String(Rc::new(message.into())),
        );

        error
    }

    fn init(global_this: &JsObject<'a>) -> Errors<'a> {
        let error = errors::JsError::bind(None);

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
