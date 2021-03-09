#[derive(Debug)]
pub struct SyntaxError {
    pub message: String,
}

#[derive(Debug)]
pub struct InternalError {
    pub message: String,
}

impl SyntaxError {
    pub(crate) fn new(message: impl Into<String>) -> SyntaxError {
        SyntaxError {
            message: message.into(),
        }
    }
}

impl InternalError {
    pub(crate) fn new(message: impl Into<String>) -> InternalError {
        InternalError {
            message: message.into(),
        }
    }
}

impl<T> From<SyntaxError> for Result<T> {
    fn from(syntax_error: SyntaxError) -> Self {
        Err(CompilerError::SyntaxError(syntax_error))
    }
}

#[derive(Debug)]
pub enum CompilerError {
    SyntaxError(SyntaxError),
    InternalError(InternalError),
}

pub type Result<T> = std::result::Result<T, CompilerError>;

impl<'a> From<CompilerError> for anyhow::Error {
    fn from(err: CompilerError) -> Self {
        match err {
            CompilerError::SyntaxError(err) => {
                anyhow::Error::msg(format!("SyntaxError: {}", err.message))
            }
            CompilerError::InternalError(err) => {
                anyhow::Error::msg(format!("InternalError: {}", err.message))
            }
        }
    }
}
