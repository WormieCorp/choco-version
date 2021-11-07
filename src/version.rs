// Copyright (c) 2021 Kim J. Nordmo and WormieCorp.
// Licensed under the Apache 2.0 license. See LICENSE.txt file in the project

use std::cmp::Ordering;
use std::fmt::Display;
use std::str::FromStr;

use lazy_static::lazy_static;
use regex::Regex;

use crate::errors::VersionParseError;

lazy_static! {
  static ref SEMANTIC_VERSION_REGEX: Regex = Regex::new(r"(?i)^(?P<Version>\d+(\s*\.\s*\d+){0,3})(?P<Prerelease>-[a-z][0-9a-z-]*)?(?P<PackageVersion>_\d+)?$").unwrap();
}

#[derive(Default, Debug, Clone, Eq)]
pub struct ChocoVersion {
    original_version: String,

    major: i32,
    minor: i32,
    build: Option<i32>,
    revision: Option<i32>,

    special_version: Option<String>,
    package_release_version: i32,
}

impl ChocoVersion {
    #[inline]
    pub fn new(major: i32, minor: i32) -> Self {
        Self {
            major: normalize_number(major, true).unwrap(),
            minor: normalize_number(minor, true).unwrap(),
            ..Default::default()
        }
    }

    pub fn set_major(&mut self, major: i32) {
        let major = normalize_number(major, true).unwrap();
        if self.major != major {
            self.major = major;
            self.original_version.clear();
        }
    }

    pub fn set_minor(&mut self, minor: i32) {
        let minor = normalize_number(minor, true).unwrap();
        if self.minor != minor {
            self.minor = minor;
            self.original_version.clear();
        }
    }

    #[inline]
    pub fn with_build(mut self, build: i32) -> Self {
        self.set_build(build);

        self
    }

    pub fn set_build(&mut self, build: i32) {
        let build = normalize_number(build, true);
        if self.build != build {
            self.build = build;
            self.original_version.clear();
        }
    }

    #[inline]
    pub fn with_revision(mut self, revision: i32) -> Self {
        self.set_revision(revision);

        self
    }

    pub fn set_revision(&mut self, revision: i32) {
        let revision = normalize_number(revision, true);
        if self.revision != revision {
            self.revision = revision;
            self.original_version.clear();

            if self.build.is_none() {
                self.set_build(0);
            }
        }
    }

    #[inline]
    pub fn with_special_version<S: AsRef<str>>(mut self, special_version: S) -> Self {
        self.set_special_version(special_version);

        self
    }

    pub fn set_special_version<S: AsRef<str>>(&mut self, special_version: S) {
        let special_version = {
            let special_version = special_version.as_ref().trim();
            if special_version.is_empty() {
                None
            } else {
                Some(special_version.to_string())
            }
        };

        if self.special_version != special_version {
            self.special_version = special_version;
            self.original_version.clear();
        }
    }

    #[inline]
    pub fn with_package_release_version(mut self, package_release_version: i32) -> Self {
        self.set_package_release_version(package_release_version);

        self
    }

    pub fn set_package_release_version(&mut self, package_release_version: i32) {
        let package_release_version = normalize_number(package_release_version, true).unwrap();

        if self.package_release_version != package_release_version {
            self.package_release_version = package_release_version;
            self.original_version.clear();
        }
    }

    #[inline]
    fn with_original_version<S: AsRef<str>>(mut self, original_version: S) -> Self {
        self.set_original_version(original_version);

        self
    }

    fn set_original_version<S: AsRef<str>>(&mut self, original_version: S) {
        let original_version = original_version.as_ref().trim();

        if original_version.is_empty() {
            self.original_version.clear();
        } else if self.original_version.as_str() != original_version {
            self.original_version = original_version.to_string();
        };
    }

    /// ```rust
    /// use choco_version::prelude::*;
    ///
    /// let version = ChocoVersion::new(5, 0).with_special_version("Beta");
    ///
    /// println!("{}", version.version());
    /// ```
    pub fn version(&self) -> String {
        let mut result = format!("{}.{}", self.major, self.minor);
        if let Some(build) = self.build {
            result.push_str(&format!(".{}", build));

            if let Some(revision) = self.revision {
                result.push_str(&format!(".{}", revision));
            }
        }

        result
    }

    #[inline]
    pub fn major(&self) -> i32 {
        self.major
    }

    #[inline]
    pub fn minor(&self) -> i32 {
        self.minor
    }

    #[inline]
    pub fn build(&self) -> i32 {
        if let Some(build) = self.build {
            build
        } else {
            0
        }
    }

    #[inline]
    pub fn revision(&self) -> i32 {
        if let Some(revision) = self.revision {
            revision
        } else {
            0
        }
    }

    #[inline]
    pub fn special_version(&self) -> &str {
        if let Some(ref special_version) = self.special_version {
            special_version
        } else {
            ""
        }
    }

    #[inline]
    pub fn package_release_version(&self) -> i32 {
        self.package_release_version
    }

    pub fn to_normalized_string(&self) -> String {
        let mut result = format!("{}.{}.{}", self.major, self.minor, self.build());

        if self.revision() > 0 {
            result.push_str(&format!(".{}", self.revision()));
        }

        if let Some(ref special_version) = self.special_version {
            result.push_str(&format!("-{}", special_version));
        }

        if self.package_release_version > 0 {
            result.push_str(&format!("_{}", self.package_release_version));
        }

        result
    }
}

impl Display for ChocoVersion {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        if self.original_version.is_empty() {
            write!(
                fmt,
                "{}",
                build_original_version(
                    self.major,
                    self.minor,
                    self.build,
                    self.revision,
                    self.special_version.as_ref(),
                    self.package_release_version
                )
            )
        } else {
            write!(fmt, "{}", self.original_version.clone())
        }
    }
}

impl FromStr for ChocoVersion {
    type Err = VersionParseError;

    /// ```rust
    /// use choco_version::prelude::*;
    ///
    /// let version = ChocoVersion::from_str("1.0.0.0-beta-33").unwrap();
    ///
    /// println!("{}", version);
    /// ```
    #[inline]
    fn from_str(version: &str) -> std::result::Result<Self, <Self as std::str::FromStr>::Err> {
        try_parse_internal(version, &SEMANTIC_VERSION_REGEX)
    }
}

impl PartialEq for ChocoVersion {
    #[inline]
    fn eq(&self, right: &ChocoVersion) -> bool {
        self.major() == right.major()
            && self.minor() == right.minor()
            && self.build() == right.build()
            && self.revision() == right.revision()
            && self.special_version().to_lowercase() == right.special_version().to_lowercase()
            && self.package_release_version() == right.package_release_version()
    }
}

impl PartialOrd for ChocoVersion {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> std::option::Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ChocoVersion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.eq(other) {
            return Ordering::Equal;
        }

        let mut result = self.major.cmp(&other.major);

        if result != Ordering::Equal {
            return result;
        }

        result = self.minor.cmp(&other.minor);

        if result != Ordering::Equal {
            return result;
        }

        result = self.build().cmp(&other.build());

        if result != Ordering::Equal {
            return result;
        }

        result = self.revision().cmp(&other.revision());

        if result != Ordering::Equal {
            return result;
        }

        result = self
            .package_release_version
            .cmp(&other.package_release_version);

        if result != Ordering::Equal {
            return result;
        }

        let pre1 = self.special_version().to_lowercase();
        let pre2 = other.special_version().to_lowercase();

        if pre1.is_empty() && pre2.is_empty() {
            Ordering::Equal
        } else if pre1.is_empty() {
            Ordering::Greater
        } else if pre2.is_empty() {
            Ordering::Less
        } else {
            pre1.cmp(&pre2)
        }
    }
}

fn build_original_version<T: AsRef<str>>(
    major: i32,
    minor: i32,
    build: Option<i32>,
    revision: Option<i32>,
    special_version: Option<T>,
    package_release_version: i32,
) -> String {
    let mut version = format!("{}.{}", major, minor);

    if let Some(build) = build {
        version.push_str(&format!(".{}", build));

        if let Some(revision) = revision {
            version.push_str(&format!(".{}", revision));
        }
    }

    if let Some(special_version) = special_version {
        version.push_str(&format!("-{}", special_version.as_ref()));
    }

    if package_release_version > 0 {
        version.push_str(&format!("_{}", package_release_version));
    }

    version
}

#[inline]
fn normalize_number(num: i32, is_required: bool) -> Option<i32> {
    if num <= 0 && is_required {
        Some(0)
    } else if num <= 0 {
        None
    } else {
        Some(num)
    }
}

fn try_parse_internal(
    version_string: &str,
    regex: &Regex,
) -> Result<ChocoVersion, VersionParseError> {
    let version_string = version_string.trim();

    if version_string.is_empty() {
        return Err(VersionParseError::new("Version value can not be empty!"));
    }

    let captures = regex.captures(version_string).ok_or_else(|| {
        VersionParseError::new(&format!(
            "{} is not a valid version string.",
            version_string
        ))
    })?;

    let mut version = {
        let version_string = captures.name("Version").ok_or_else(|| {
            VersionParseError::new(&format!("{} is not a valid version sring.", version_string))
        })?;

        let (major, minor, build, revision) = try_parse_internal_version(version_string.as_str())?;

        let mut version = ChocoVersion::new(major, minor);
        version.build = build;
        version.revision = revision;

        version
    };

    let special_version = captures
        .name("Prerelease")
        .map(|special_version| special_version.as_str().trim_start_matches('-'));
    version.set_special_version(special_version.unwrap_or_default());

    let package_release = if let Some(package_release) = captures.name("PackageVersion") {
        package_release.as_str().trim_start_matches('_').parse()?
    } else {
        0
    };

    version.set_package_release_version(package_release);

    Ok(version.with_original_version(version_string))
}

fn try_parse_internal_version(
    version: &str,
) -> Result<(i32, i32, Option<i32>, Option<i32>), VersionParseError> {
    let parts: Vec<&str> = version.split_terminator('.').collect();

    if parts.len() < 2 || parts.len() > 4 {
        return Err(VersionParseError::new(format!(
            "Invalid number of version parts, Expected 2-4 parts, found '{}'. Unable to continue \
             parsing!",
            parts.len()
        )));
    }

    let mut iter = parts.iter();

    let major = iter.next().unwrap().parse()?;
    let minor = iter.next().unwrap().parse()?;
    let build = iter.next().and_then(|s| s.parse().ok());
    let revision = iter.next().and_then(|s| s.parse().ok());

    Ok((major, minor, build, revision))
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    #[test]
    fn new_with_only_major_and_minor_version() {
        let version = ChocoVersion::new(5, 2);

        assert_eq!(version.major(), 5);
        assert_eq!(version.minor(), 2);
        assert_eq!(version.build(), 0);
        assert_eq!(version.revision(), 0);
        assert_eq!(version.special_version(), "");
        assert_eq!(version.package_release_version(), 0);
        assert_eq!(version.to_string(), "5.2");
        assert_eq!(version.to_normalized_string(), "5.2.0");
    }

    #[test]
    fn new_with_additional_build_version() {
        let version = ChocoVersion::new(2, 3).with_build(55);

        assert_eq!(version.major(), 2);
        assert_eq!(version.minor(), 3);
        assert_eq!(version.build(), 55);
        assert_eq!(version.revision(), 0);
        assert_eq!(version.special_version(), "");
        assert_eq!(version.package_release_version(), 0);
        assert_eq!(version.to_string(), "2.3.55");
        assert_eq!(version.to_normalized_string(), "2.3.55");
    }

    #[test]
    fn new_with_additional_build_and_revision_version() {
        let version = ChocoVersion::new(1, 5).with_build(3).with_revision(3);

        assert_eq!(version.major(), 1);
        assert_eq!(version.minor(), 5);
        assert_eq!(version.build(), 3);
        assert_eq!(version.revision(), 3);
        assert_eq!(version.special_version(), "");
        assert_eq!(version.package_release_version(), 0);
        assert_eq!(version.to_string(), "1.5.3.3");
        assert_eq!(version.to_normalized_string(), "1.5.3.3");
    }

    #[test]
    fn new_with_additional_revision_version() {
        let version = ChocoVersion::new(1, 5).with_revision(3);

        assert_eq!(version.major(), 1);
        assert_eq!(version.minor(), 5);
        assert_eq!(version.build(), 0);
        assert_eq!(version.revision(), 3);
        assert_eq!(version.special_version(), "");
        assert_eq!(version.package_release_version(), 0);
        assert_eq!(version.to_string(), "1.5.0.3");
        assert_eq!(version.to_normalized_string(), "1.5.0.3");
    }

    #[test]
    fn new_with_additional_special_version() {
        let version = ChocoVersion::new(2, 0).with_special_version("alpha-55");

        assert_eq!(version.major(), 2);
        assert_eq!(version.minor(), 0);
        assert_eq!(version.build(), 0);
        assert_eq!(version.revision(), 0);
        assert_eq!(version.special_version(), "alpha-55");
        assert_eq!(version.package_release_version(), 0);
        assert_eq!(version.to_string(), "2.0-alpha-55");
        assert_eq!(version.to_normalized_string(), "2.0.0-alpha-55");
    }

    #[test]
    fn new_with_additional_package_release_version() {
        let version = ChocoVersion::new(3, 5).with_package_release_version(5);

        assert_eq!(version.major(), 3);
        assert_eq!(version.minor(), 5);
        assert_eq!(version.build(), 0);
        assert_eq!(version.revision(), 0);
        assert_eq!(version.special_version(), "");
        assert_eq!(version.package_release_version(), 5);
        assert_eq!(version.to_string(), "3.5_5");
        assert_eq!(version.to_normalized_string(), "3.5.0_5");
    }

    #[rstest(
        version,
        case("1.0"),
        case("1.0.0"),
        case("1.0.0.0"),
        case("1.0_1"),
        case("1.0-alpha"),
        case("1.0-alpha_1"),
        case("1.0.0-beta"),
        case("3.0.1.2"),
        case("2.1.4.3-pre-1"),
        case("3.5"),
        case("2.5"),
        case("2.5.56-alpha_5"),
        case("55.223.5_2")
    )]
    fn to_string_with_original_version_string(version: &str) {
        let choco_version = ChocoVersion::new(2, 5).with_original_version(version);

        assert_eq!(choco_version.to_string(), version);
    }

    #[rstest(
    major,
    minor,
    build,
    revision,
    special_version,
    package_release,
    expected,
    case(1, 1, None, None, None, 0, "1.1"),
    case(1, 1, None, None, None, 6, "1.1_6"),
    case(1, 2, None, None, Some("alpha"), 0, "1.2-alpha"),
    case(1, 2, None, None, Some("alpha"), 77, "1.2-alpha_77"),
    case(2, 5, Some(1), None, None, 0, "2.5.1"),
    case(2, 5, Some(1), None, None, 66, "2.5.1_66"),
    case(2, 5, Some(1), None, Some("Beta"), 0, "2.5.1-Beta"),
    case(2, 5, Some(1), None, Some("Beta"), 3, "2.5.1-Beta_3"),
    case(2, 5, Some(2), Some(5), None, 0, "2.5.2.5"),
    case(2, 5, Some(2), Some(5), None, 5, "2.5.2.5_5"),
    case(2, 5, Some(2), Some(5), Some("PreRelease"), 5, "2.5.2.5-PreRelease_5"),
    case(2, 5, Some(2), Some(5), Some("PreRelease"), 0, "2.5.2.5-PreRelease"),
    case(2, 5, None, Some(2), None, 0, "2.5.0.2"),
    case(2, 5, None, Some(2), None, 6, "2.5.0.2_6"),
    case(2, 5, None, Some(2), Some("Beta"), 0, "2.5.0.2-Beta"),
    case(2, 5, None, Some(2), Some("Beta"), 6, "2.5.0.2-Beta_6"),
    case(-1, -1, Some(-1), Some(-1), Some(""), -1, "0.0.0.0"),
    case(-1, -1, Some(-1), Some(-1), Some("  "), -1, "0.0.0.0"),
    case(1, 0, None, None, None ,0, "1.0"),
    case(1, 0, None, None, None, 2, "1.0_2"),
    case(1, 0, Some(3), Some(120), None, 0, "1.0.3.120"),
    case(1, 0, Some(3), Some(120), Some("alpha"), 0, "1.0.3.120-alpha"),
    case(1, 0, Some(3), Some(120), Some("rc-2"), 0, "1.0.3.120-rc-2"),
    case(1, 0, Some(3), Some(120), Some("rc-2"), 1, "1.0.3.120-rc-2_1")
  )]
    fn to_string_without_original_version_string(
        major: i32,
        minor: i32,
        build: Option<i32>,
        revision: Option<i32>,
        special_version: Option<&str>,
        package_release: i32,
        expected: &str,
    ) {
        let mut choco_version =
            ChocoVersion::new(major, minor).with_package_release_version(package_release);
        if let Some(build) = build {
            choco_version.set_build(build);
        }
        if let Some(revision) = revision {
            choco_version.set_revision(revision);
        }
        if let Some(special_version) = special_version {
            choco_version.set_special_version(special_version);
        }

        assert_eq!(choco_version.to_string(), expected);
    }

    #[test]
    fn compare_same_versions_should_be_true() {
        let left = ChocoVersion::from_str("5.0.2.1-alpha_55").unwrap();
        let right = ChocoVersion::from_str("5.0.2.1-alpha_55").unwrap();

        assert!(left == right);
        assert!(left.eq(&right));
    }

    #[rstest(
        version_string,
        version_value,
        special_value,
        package_release_version,
        case("1.0.0", "1.0.0", "", 0),
        case("1.3.2-CTP-2-Refresh-Alpha", "1.3.2", "CTP-2-Refresh-Alpha", 0)
    )]
    fn parse_parses_values_correctly(
        version_string: &str,
        version_value: &str,
        special_value: &str,
        package_release_version: i32,
    ) {
        let version = ChocoVersion::from_str(version_string).unwrap();

        assert_eq!(version.version(), version_value);
        assert_eq!(version.package_release_version(), package_release_version);
        assert_eq!(version.to_string(), version_string);
        assert_eq!(version.special_version(), special_value);
    }

    #[rstest(
        version_string,
        message,
        case(
            "1",
            "Invalid number of version parts, Expected 2-4 parts, found '1'. Unable to continue \
             parsing!"
        ),
        case("1beta", "1beta is not a valid version string."),
        case("1.2Av^c", "1.2Av^c is not a valid version string."),
        case("1.2..", "1.2.. is not a valid version string."),
        case("1.2.3.4.5", "1.2.3.4.5 is not a valid version string."),
        case("1.2.3.Beta", "1.2.3.Beta is not a valid version string."),
        case(
            "1.2.3.4This version is full of awesomeness!!",
            "1.2.3.4This version is full of awesomeness!! is not a valid version string."
        ),
        case("So.is.this", "So.is.this is not a valid version string."),
        case("1.34.2Alpha", "1.34.2Alpha is not a valid version string."),
        case(
            "1.34.2Release Candidate",
            "1.34.2Release Candidate is not a valid version string."
        ),
        case("1.4.7-", "1.4.7- is not a valid version string."),
        case("1.4.7_", "1.4.7_ is not a valid version string."),
        case("1.4.7_2_", "1.4.7_2_ is not a valid version string."),
        case("", "Version value can not be empty!")
    )]
    fn parse_returns_error_on_invalid_version(version_string: &str, message: &str) {
        let result = ChocoVersion::from_str(version_string);

        assert_eq!(result, Err(VersionParseError::new(message)));
    }

    #[rstest(
        version_string,
        expected_version,
        expected_special_version,
        package_release,
        case("1.022", "1.22", "", 0),
        case("1.022_1", "1.22", "", 1),
        case("23.2.3", "23.2.3", "", 0),
        case("1.3.42.10133", "1.3.42.10133", "", 0)
    )]
    fn parse_reads_legacy_style_version_numbers(
        version_string: &str,
        expected_version: &str,
        expected_special_version: &str,
        package_release: i32,
    ) {
        let actual = ChocoVersion::from_str(version_string).unwrap();

        assert_eq!(actual.version(), expected_version);
        assert_eq!(actual.special_version(), expected_special_version);
        assert_eq!(actual.package_release_version(), package_release);
    }

    #[rstest(
        version_string,
        expected_version,
        expected_special_version,
        package_release,
        case("1.022-Beta", "1.22", "Beta", 0),
        case("1.022-Beta_1", "1.22", "Beta", 1),
        case("23.2.3-Alpha", "23.2.3", "Alpha", 0),
        case("1.3.42.10133-PreRelease", "1.3.42.10133", "PreRelease", 0),
        case("1.3.42.200930-RC-2", "1.3.42.200930", "RC-2", 0)
    )]
    fn parse_reads_semver_and_hybrid_semver_version_numbers(
        version_string: &str,
        expected_version: &str,
        expected_special_version: &str,
        package_release: i32,
    ) {
        let actual = ChocoVersion::from_str(version_string).unwrap();

        assert_eq!(actual.version(), expected_version);
        assert_eq!(actual.special_version(), expected_special_version);
        assert_eq!(actual.package_release_version(), package_release);
    }

    #[rstest(
        version_string,
        expected_version,
        expected_special_version,
        package_release,
        case("  1.022-Beta", "1.22", "Beta", 0),
        case("  1.022-Beta_1", "1.22", "Beta", 1),
        case("23.2.3-Alpha  ", "23.2.3", "Alpha", 0),
        case("    1.3.42.10133-PreRelease  ", "1.3.42.10133", "PreRelease", 0)
    )]
    fn parse_reads_semver_with_whitespace(
        version_string: &str,
        expected_version: &str,
        expected_special_version: &str,
        package_release: i32,
    ) {
        let actual = ChocoVersion::from_str(version_string).unwrap();

        assert_eq!(actual.version(), expected_version);
        assert_eq!(actual.special_version(), expected_special_version);
        assert_eq!(actual.package_release_version(), package_release);
    }

    #[rstest(
        version_a,
        version_b,
        case("1.0", "1.0.1"),
        case("1.23", "1.231"),
        case("1.4.5.6", "1.45.6"),
        case("1.4.5.6", "1.4.5.60"),
        case("1.01", "1.10"),
        case("1.01-alpha", "1.10-beta"),
        case("1.0", "1.0_1"),
        case("1.0_1", "1.0_2"),
        case("1.01.0-RC-1", "1.10.0-rc-2"),
        case("1.01-RC-1", "1.01"),
        case("1.01", "1.2-preview"),
        case("56.232-previews", "70.0"),
        case("5.0.0-beta", "5.0.0")
    )]
    fn semver_less_than_and_greater_than_comparison_works(version_a: &str, version_b: &str) {
        let item_a = ChocoVersion::from_str(version_a).unwrap();
        let item_b = ChocoVersion::from_str(version_b).unwrap();

        assert_eq!(item_a.cmp(&item_b), Ordering::Less);
        assert_eq!(item_a.partial_cmp(&item_b), Some(Ordering::Less));
        assert_eq!(item_b.cmp(&item_a), Ordering::Greater);
        assert_eq!(item_b.partial_cmp(&item_a), Some(Ordering::Greater));
        assert!(!item_a.eq(&item_b));
        assert!(!item_b.eq(&item_a));
    }

    #[rstest(
        version_a,
        version_b,
        case("1.0", "1.0.0.0"),
        case("1.23.01", "1.23.1"),
        case("1.45.6", "1.45.6.0"),
        case("1.0", "1.0_0"),
        case("1.45.6-Alpha", "1.45.6-Alpha"),
        case("1.6.2-BeTa", "1.6.02-beta"),
        case("22.3.07     ", "22.3.07"),
        case("5.0.2.5", "5.0.2.5")
    )]
    fn semver_equals_comparison_works(version_a: &str, version_b: &str) {
        let item_a = ChocoVersion::from_str(version_a).unwrap();
        let item_b = ChocoVersion::from_str(version_b).unwrap();

        assert_eq!(item_a.cmp(&item_b), Ordering::Equal);
        assert_eq!(item_b.cmp(&item_a), Ordering::Equal);
        assert!(item_a.eq(&item_b));
        assert!(item_b.eq(&item_a));
    }

    #[rstest(
        version_string,
        expected_version,
        case("1.0", "1.0.0"),
        case("1.7", "1.7.0"),
        case("1.0.0.0", "1.0.0"),
        case("1.0.0", "1.0.0"),
        case("1.2.3", "1.2.3"),
        case("1.2.03", "1.2.3"),
        case("1.2.0.4", "1.2.0.4"),
        case("1.2.3.4", "1.2.3.4"),
        case("1.2_1", "1.2.0_1"),
        case("1.2-special_1", "1.2.0-special_1"),
        case("1.2-special", "1.2.0-special"),
        case("1.2.3-special", "1.2.3-special"),
        case("1.2.3.5-special", "1.2.3.5-special"),
        case("1.2.0.5-special", "1.2.0.5-special")
    )]
    fn to_normalized_string_normalizes_to_minimum_of_3_digits(
        version_string: &str,
        expected_version: &str,
    ) {
        let version = ChocoVersion::from_str(version_string).unwrap();

        assert_eq!(version.to_normalized_string(), expected_version);
    }
}
