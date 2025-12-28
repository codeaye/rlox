use std::sync::Arc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
#[error("unable to read input to memory")]
#[diagnostic(severity(Error))]
pub struct ProcessError {
    #[help]
    pub advice: String,
}

#[derive(Debug, Diagnostic, Error)]
#[error("compile time error")]
#[diagnostic(severity(Error))]
pub struct CompileTimeError {
    #[help]
    pub advice: String,
    #[source_code]
    pub source_code: Arc<str>,
    #[label]
    pub err_span: SourceSpan,
}

#[derive(Debug, Diagnostic, Error)]
#[error("runtime error")]
#[diagnostic(severity(Error))]
pub struct RuntimeError {
    #[help]
    pub advice: String,
}
