// Copyright (c) 2021 Kim J. Nordmo and WormieCorp.
// Licensed under the Apache 2.0 license. See LICENSE.txt file in the project

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::ParseIntError;

/// Represents a error that occurred while parsing any versions from a string.
#[derive(Debug, PartialEq, Eq)]
pub struct VersionParseError {
    message: String,
}

impl VersionParseError {
    /// Creates a new instnance of a single parsing error with the specified
    /// message.
    pub fn new<S: AsRef<str>>(message: S) -> VersionParseError {
        VersionParseError {
            message: message.as_ref().into(),
        }
    }

    /// The message that is intended to explain why an error occurred.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl From<ParseIntError> for VersionParseError {
    fn from(val: ParseIntError) -> Self {
        VersionParseError::new(val.to_string())
    }
}

impl From<&str> for VersionParseError {
    fn from(val: &str) -> Self {
        VersionParseError::new(val)
    }
}

impl Display for VersionParseError {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.message())
    }
}

impl Error for VersionParseError {}
