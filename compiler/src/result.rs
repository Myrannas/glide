use crate::value::RuntimeValue;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Stack {
    entries: Vec<String>,
}

impl Display for Stack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for entry in &self.entries {
            f.write_fmt(format_args!("{}\n", entry))?;
        }

        Ok(())
    }
}

impl From<Vec<String>> for Stack {
    fn from(entries: Vec<String>) -> Self {
        Stack { entries }
    }
}

#[derive(Debug)]
pub enum ExecutionError<'a> {
    Thrown(RuntimeValue<'a>, Option<Stack>),
    InternalError(InternalError),
    SyntaxError(SyntaxError),
}

#[derive(Debug)]
pub enum StaticExecutionError {
    SyntaxError(SyntaxError),
    InternalError(InternalError),
}

#[derive(Debug)]
pub struct InternalError {
    message: String,
    stack: Option<Stack>,
}

#[derive(Debug)]
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

    pub(crate) fn throw(runtime_value: RuntimeValue<'a>) -> Self {
        ExecutionError::Thrown(runtime_value, None)
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

impl<'a> From<RuntimeValue<'a>> for ExecutionError<'a> {
    fn from(err: RuntimeValue<'a>) -> Self {
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

impl<'a> From<ExecutionError<'a>> for anyhow::Error {
    fn from(err: ExecutionError) -> Self {
        match err {
            ExecutionError::SyntaxError(err) => {
                anyhow::Error::msg(format!("SyntaxError: {}", err.message))
            }
            ExecutionError::InternalError(err) => {
                let rendered_error = anyhow::Error::msg(format!("InternalError: {}", err.message));

                if let Some(stack) = err.stack {
                    rendered_error.context(stack)
                } else {
                    rendered_error
                }
            }
            ExecutionError::Thrown(err, stack) => {
                let rendered_error = anyhow::Error::msg(format!("{:?}", err));

                if let Some(stack) = stack {
                    rendered_error.context(stack)
                } else {
                    rendered_error
                }
            }
        }
    }
}

pub type JsResult<'a, T = RuntimeValue<'a>> = std::result::Result<T, ExecutionError<'a>>;
pub type StaticJsResult<T> = std::result::Result<T, StaticExecutionError>;

impl From<InternalError> for anyhow::Error {
    fn from(err: InternalError) -> Self {
        anyhow::Error::msg(err.message)
    }
}
