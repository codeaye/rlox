use std::sync::Arc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
#[error("invalid input path")]
#[diagnostic(severity(Error))]
pub struct InvalidInputPath {
    #[help]
    pub advice: String,
}

#[derive(Debug, Diagnostic, Error)]
#[error("source must be in UTF-8")]
#[diagnostic(severity(Error))]
pub struct InvalidSource;

#[derive(Debug, Diagnostic, Error)]
#[error("unknown character")]
#[diagnostic(severity(Error))]
pub struct UnknownCharacter {
    #[help]
    pub advice: String,
    #[source_code]
    pub source_code: Arc<str>,
    #[label]
    pub err_span: SourceSpan,
}

#[derive(Debug, Diagnostic, Error)]
#[error("unterminated string found")]
#[diagnostic(severity(Error))]
#[diagnostic(help("the terminating character '\"' was not found for this string"))]
pub struct UnterminatedString {
    #[source_code]
    pub source_code: Arc<str>,
    #[label]
    pub err_span: SourceSpan,
}

#[derive(Debug, Diagnostic, Error)]
#[error("unsupported type conversion")]
#[diagnostic(severity(Error))]
pub struct UnsupportedTypeConversion {
    #[help]
    pub advice: String,
}

#[derive(Debug, Diagnostic, Error)]
#[error("token not found")]
#[diagnostic(severity(Error))]
pub struct TokenNotFound {
    #[help]
    pub advice: String,
    #[source_code]
    pub source_code: Arc<str>,
    #[label]
    pub err_span: SourceSpan,
}

#[derive(Debug, Diagnostic, Error)]
#[error("couldn't parse number")]
#[diagnostic(severity(Error))]
pub struct ParseNumberError {
    #[source_code]
    pub source_code: Arc<str>,
    #[label]
    pub err_span: SourceSpan,
}

#[derive(Debug, Diagnostic, Error)]
#[error("expected expression")]
#[diagnostic(severity(Error))]
pub struct ExpectExpression {
    #[source_code]
    pub source_code: Arc<str>,
    #[label]
    pub err_span: SourceSpan,
}

// #[derive(Debug, Diagnostic, Error)]
// #[error("stack not deep enough to peek")]
// #[diagnostic(severity(Error))]
// pub struct StackPeekError;
