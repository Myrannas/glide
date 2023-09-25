use crate::debugging::{DebugRepresentation, Renderer};
use crate::values::nan::Value;
use crate::JsThread;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone)]
pub struct Stack {
    pub(crate) entries: Vec<StackTraceFrame>,
}

#[derive(Debug, Clone)]
pub struct StackTraceFrame {
    pub(crate) function: String,
    pub(crate) offset: usize,
}

impl Display for Stack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for entry in &self.entries {
            f.write_fmt(format_args!("{} ({})\n", entry.function, entry.offset))?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub enum ExecutionError<'a> {
    Thrown(Value<'a>, Option<Stack>),
    InternalError(InternalError),
    SyntaxError(SyntaxError),
    TypeError(String),
    ReferenceError(String),
}

impl Debug for ExecutionError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionError::Thrown(_value, stack) => {
                f.write_fmt(format_args!("Thrown {:?}", stack))
            }
            ExecutionError::InternalError(err) => {
                f.write_fmt(format_args!("InternalError: {:?}", err))
            }
            ExecutionError::SyntaxError(err) => f.write_fmt(format_args!("SyntaxError: {:?}", err)),
            ExecutionError::TypeError(err) => f.write_fmt(format_args!("TypeError: {:?}", err)),
            ExecutionError::ReferenceError(err) => {
                f.write_fmt(format_args!("ReferenceError: {}", err))
            }
        }
    }
}

#[derive(Debug)]
pub enum StaticExecutionError {
    SyntaxError(SyntaxError),
    InternalError(InternalError),
}

#[derive(Debug, Clone)]
pub struct InternalError {
    message: String,
    stack: Option<Stack>,
}

#[derive(Debug, Clone)]
pub struct SyntaxError {
    message: String,
    stack: Option<Stack>,
}

impl InternalError {
    pub fn new(message: impl Into<String>, stack: impl Into<Stack>) -> Self {
        InternalError {
            message: message.into(),
            stack: Some(stack.into()),
        }
    }

    pub fn new_stackless(message: impl Into<String>) -> Self {
        InternalError {
            message: message.into(),
            stack: None,
        }
    }
}

impl SyntaxError {
    pub fn new(message: impl Into<String>) -> Self {
        SyntaxError {
            message: message.into(),
            stack: None,
        }
    }
}

impl<'a> ExecutionError<'a> {
    pub(crate) fn fill_stack_trace(self, stack_trace: impl Into<Stack>) -> Self {
        match self {
            ExecutionError::Thrown(err, None) => {
                ExecutionError::Thrown(err, Some(stack_trace.into()))
            }
            ExecutionError::InternalError(InternalError {
                message,
                stack: Option::None,
            }) => ExecutionError::InternalError(InternalError {
                message,
                stack: Some(stack_trace.into()),
            }),
            other => other,
        }
    }

    // #[allow(clippy::if)]
    pub fn render(self, thread: &mut JsThread<'a>) -> anyhow::Error {
        match self {
            ExecutionError::SyntaxError(err) => {
                anyhow::Error::msg(format!("SyntaxError: {}", err.message))
            }
            ExecutionError::InternalError(err) => {
                let rendered_error = anyhow::Error::msg(format!("InternalError: {}", err.message));

                if let Some(stack) = err.stack {
                    return rendered_error.context(stack);
                }

                rendered_error
            }
            ExecutionError::Thrown(err, stack) => {
                let string_value = err
                    .to_string(thread)
                    .unwrap_or(thread.realm.constants.to_string);
                let str = thread.get_realm().get_string(string_value);
                let rendered_error = anyhow::Error::msg(str.to_string());

                if let Some(stack) = stack {
                    return rendered_error.context(stack);
                }

                rendered_error
            }
            ExecutionError::TypeError(err) => {
                let rendered_error = anyhow::Error::msg(err.to_string());

                rendered_error
            }
            ExecutionError::ReferenceError(err) => anyhow::Error::msg(err),
        }
    }
}

impl<T> From<SyntaxError> for StaticJsResult<T> {
    fn from(err: SyntaxError) -> Self {
        Err(StaticExecutionError::SyntaxError(err))
    }
}

impl<T> From<InternalError> for StaticJsResult<T> {
    fn from(err: InternalError) -> Self {
        Err(StaticExecutionError::InternalError(err))
    }
}

impl<'a, T> From<InternalError> for JsResult<'a, T> {
    fn from(err: InternalError) -> Self {
        Err(ExecutionError::InternalError(err))
    }
}

impl<'a> From<InternalError> for ExecutionError<'a> {
    fn from(err: InternalError) -> Self {
        ExecutionError::InternalError(err)
    }
}

impl<'a> From<Value<'a>> for ExecutionError<'a> {
    fn from(err: Value<'a>) -> Self {
        ExecutionError::Thrown(err, None)
    }
}

impl<'a> From<StaticExecutionError> for ExecutionError<'a> {
    fn from(err: StaticExecutionError) -> Self {
        match err {
            StaticExecutionError::SyntaxError(err) => ExecutionError::SyntaxError(err),
            StaticExecutionError::InternalError(err) => ExecutionError::InternalError(err),
        }
    }
}

impl From<StaticExecutionError> for anyhow::Error {
    fn from(err: StaticExecutionError) -> Self {
        match err {
            StaticExecutionError::SyntaxError(err) => {
                anyhow::Error::msg(format!("SyntaxError: {}", err.message))
            }
            StaticExecutionError::InternalError(err) => {
                anyhow::Error::msg(format!("InternalError: {}", err.message))
            }
        }
    }
}

pub type JsResult<'a, T = Value<'a>> = std::result::Result<T, ExecutionError<'a>>;
pub type StaticJsResult<T> = std::result::Result<T, StaticExecutionError>;

impl From<InternalError> for anyhow::Error {
    fn from(err: InternalError) -> Self {
        anyhow::Error::msg(err.message)
    }
}

impl<'a> DebugRepresentation<'a> for ExecutionError<'a> {
    fn render(&self, renderer: &mut Renderer<'a, '_, '_, '_>) -> std::fmt::Result {
        let stack = match self {
            ExecutionError::Thrown(value, stack) => {
                value.render(renderer)?;

                stack
            }
            ExecutionError::InternalError(InternalError { message, stack })
            | ExecutionError::SyntaxError(SyntaxError { message, stack }) => {
                renderer.string_literal(message)?;

                stack
            }
            ExecutionError::TypeError(v) => {
                renderer.string_literal(v);

                &None
            }
            ExecutionError::ReferenceError(err) => {
                renderer.string_literal(err);
                &None
            }
        };

        if let Some(stack) = stack {
            renderer.formatter.write_fmt(format_args!("{:?}", stack))?
        }

        Ok(())
    }
}
