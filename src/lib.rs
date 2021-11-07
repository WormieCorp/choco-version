// Copyright (c) 2021 Kim J. Nordmo and WormieCorp.
// Licensed under the Apache 2.0 license. See LICENSE.txt file in the project

//! choco_version is a small library with the purpose of porting
//! the way Chocolatey handles versioning and tasks related to that.
//!
//! That includes parsing Chocolatey compatibl strings into their own
//! structures that can be used to compare and order said versions in
//! the same manner that Chocolatey do these things.
//!
//! Additionally their are also utillities to help with what is called
//! version specifications, which can be used to validate or check if
//! some parsed versions are within a certain range (like a referenced
//! dependency in Chocolatey packages).
//!
//! ### Basic Example when parsing string
//!
//! ```rust
//! use choco_version::prelude::*;
//!
//! let version: ChocoVersion = "5.0.1".parse().unwrap();
//!
//! println!("{}", version);
//! ```
//!
//! ### Basic Example using `from_str`
//!
//! ```rust
//! use choco_version::prelude::*;
//!
//! let version = ChocoVersion::from_str("5.0.6-alpha-55").unwrap();
//!
//! println!("{}", version);
//! ```

#![cfg_attr(docsrs, feature(doc_cfg))]

pub mod errors;
pub mod version;

pub mod prelude {
    pub use std::str::FromStr;

    pub use version::ChocoVersion;

    use crate::version;
}
