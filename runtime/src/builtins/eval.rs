use crate::result::{JsResult, SyntaxError};
use crate::values::function::{CustomFunctionReference, FunctionReference};
use crate::values::nan::{Value, ValueType};
use crate::{ExecutionError, InternalError, JsFunction, JsThread};
use glide_compiler::{
    compile_eval, parse_input, CompilerError, InternalError as CompilationInternalError,
    SyntaxError as CompilationSyntaxError,
};

impl<'a> From<CompilerError> for ExecutionError<'a> {
    fn from(error: CompilerError) -> Self {
        match error {
            CompilerError::SyntaxError(CompilationSyntaxError { message }) => {
                ExecutionError::SyntaxError(SyntaxError::new(message))
            }
            CompilerError::InternalError(CompilationInternalError { message }) => {
                ExecutionError::InternalError(InternalError::new_stackless(message))
            }
        }
    }
}

pub(crate) fn eval<'a>(
    args: usize,
    frame: &mut JsThread<'a>,
    _value: Value<'a>,
    _context: Option<Value<'a>>,
) -> JsResult<'a, Option<Value<'a>>> {
    let argument = frame.read_arg(args, 0).unwrap_or_default();

    match argument.get_type() {
        ValueType::String(str) => {
            let input = frame.realm.strings.get(str).as_ref().to_owned();

            let code = match parse_input(&input) {
                Ok(code) => code,
                Err(err) => {
                    return Err(err.into());
                }
            };
            let current_function = frame.current_function();
            let locals = current_function.locals();
            let function = match compile_eval(locals.to_vec(), code) {
                Ok(code) => code,
                Err(err) => {
                    return Err(err.into());
                }
            };

            let this = frame.current_context().this();
            let context = frame.current_context().clone();

            let loaded_function = JsFunction::load(function, &mut frame.realm);
            let result = frame.call_from_native(
                this,
                FunctionReference::Custom(CustomFunctionReference {
                    function: loaded_function,
                    parent_context: context,
                }),
                0,
                false,
            )?;

            Ok(Some(result))
        }
        _ => Ok(Some(Value::UNDEFINED)),
    }
}
