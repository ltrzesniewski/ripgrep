use crate::hyperlink_aliases::HYPERLINK_PATTERN_ALIASES;
use bstr::ByteSlice;
use std::error::Error;
use std::fmt::Display;
use std::io;
use std::io::Write;
use std::path::Path;
use std::str::FromStr;
use termcolor::{HyperlinkSpec, WriteColor};

/// A hyperlink pattern with placeholders.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct HyperlinkPattern {
    parts: Vec<Part>,
    is_line_dependent: bool,
}

/// A hyperlink pattern part.
#[derive(Clone, Debug, Eq, PartialEq)]
enum Part {
    /// Static text. Can include invariant values such as the hostname.
    Text(Vec<u8>),
    /// Placeholder for the file path.
    File,
    /// Placeholder for the line number.
    Line,
    /// Placeholder for the column number.
    Column,
}

/// An error that can occur when parsing a hyperlink pattern.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HyperlinkPatternError {
    /// This occurs when the pattern syntax is not valid.
    InvalidSyntax,
    /// This occurs when the {file} placeholder is missing.
    NoFilePlaceholder,
    /// This occurs when the {line} placeholder is missing,
    /// while the {column} placeholder is present.
    NoLinePlaceholder,
    /// This occurs when an unknown placeholder is used.
    InvalidPlaceholder(String),
    /// The pattern doesn't start with a valid scheme.
    InvalidScheme,
}

/// The values to replace the pattern placeholders with.
#[derive(Clone, Debug)]
pub struct HyperlinkValues<'a> {
    file: &'a HyperlinkPath,
    line: u64,
    column: u64,
}

/// Represents the {file} part of a hyperlink.
///
/// This is the value to use as-is in the hyperlink, converted from an OS file path.
#[derive(Clone, Debug)]
pub struct HyperlinkPath(Vec<u8>);

impl HyperlinkPattern {
    /// Creates an empty hyperlink pattern.
    pub fn new() -> Self {
        HyperlinkPattern::default()
    }

    /// Creates a default pattern suitable for Unix.
    ///
    /// The returned pattern is: `file://{host}/{file}`  
    /// except on WSL, where it is: `file://wsl$/{distro}/{file}`
    #[cfg(unix)]
    pub fn new_system_default() -> Self {
        let mut pattern = Self::new();
        pattern.append_text(b"file://");

        if let Ok(wsl_distro) = std::env::var("WSL_DISTRO_NAME") {
            pattern.append_text(b"wsl$/");
            pattern.append_text(wsl_distro.as_bytes());
        } else {
            pattern.append_hostname();
        }

        pattern.append_text(b"/");
        pattern.append_placeholder(Part::File);
        pattern
    }

    /// Creates a default pattern suitable for Windows.
    ///
    /// The returned pattern is: `file:///{file}`
    #[cfg(windows)]
    pub fn new_system_default() -> Self {
        let mut pattern = Self::new();
        pattern.append_text(b"file:///");
        pattern.append_placeholder(Part::File);
        pattern
    }

    fn append_text(&mut self, text: &[u8]) {
        if let Some(Part::Text(contents)) = self.parts.last_mut() {
            contents.extend_from_slice(text);
        } else if !text.is_empty() {
            self.parts.push(Part::Text(text.to_vec()));
        }
    }

    fn append_hostname(&mut self) {
        self.append_text(
            gethostname::gethostname().to_string_lossy().as_bytes(),
        );
    }

    fn append_placeholder(&mut self, part: Part) {
        if part == Part::Line {
            self.is_line_dependent = true;
        }

        self.parts.push(part);
    }

    /// Returns true if this pattern is empty.
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    /// Returns true if the pattern can produce line-dependent hyperlinks.
    pub fn is_line_dependent(&self) -> bool {
        self.is_line_dependent
    }

    /// Renders this pattern with the given values to the given output.
    pub fn render(
        &self,
        values: &HyperlinkValues,
        output: &mut impl Write,
    ) -> io::Result<()> {
        for part in &self.parts {
            part.render(values, output)?;
        }
        Ok(())
    }

    /// Validate that the pattern is well-formed.
    fn validate(&self) -> Result<(), HyperlinkPatternError> {
        if self.parts.is_empty() {
            return Ok(());
        }

        if !self.parts.contains(&Part::File) {
            return Err(HyperlinkPatternError::NoFilePlaceholder);
        }

        if !self.is_line_dependent() && self.parts.contains(&Part::Column) {
            return Err(HyperlinkPatternError::NoLinePlaceholder);
        }

        self.validate_scheme()
    }

    /// Validate that the pattern starts with a valid scheme.
    ///
    /// A valid scheme starts with an alphabetic character, continues with
    /// a sequence of alphanumeric characters, periods, hyphens or plus signs,
    /// and ends with a colon.
    fn validate_scheme(&self) -> Result<(), HyperlinkPatternError> {
        if let Some(Part::Text(value)) = self.parts.first() {
            if let Some(colon_index) = value.find_byte(b':') {
                if value[0].is_ascii_alphabetic()
                    && value.iter().take(colon_index).all(|c| {
                        c.is_ascii_alphanumeric()
                            || matches!(c, b'.' | b'-' | b'+')
                    })
                {
                    return Ok(());
                }
            }
        }

        Err(HyperlinkPatternError::InvalidScheme)
    }
}

impl FromStr for HyperlinkPattern {
    type Err = HyperlinkPatternError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pattern = Self::new();
        let mut input = s.as_bytes();

        if let Ok(index) = HYPERLINK_PATTERN_ALIASES
            .binary_search_by_key(&input, |&(name, _)| name.as_bytes())
        {
            input = HYPERLINK_PATTERN_ALIASES[index].1.as_bytes();
        }

        loop {
            if input.is_empty() {
                break;
            }

            if input[0] == b'{' {
                // Placeholder
                let end = input
                    .find_byte(b'}')
                    .ok_or(HyperlinkPatternError::InvalidSyntax)?;

                match &input[1..end] {
                    b"file" => pattern.append_placeholder(Part::File),
                    b"line" => pattern.append_placeholder(Part::Line),
                    b"column" => pattern.append_placeholder(Part::Column),
                    b"host" => pattern.append_hostname(),
                    other => {
                        return Err(HyperlinkPatternError::InvalidPlaceholder(
                            String::from_utf8_lossy(other).to_string(),
                        ))
                    }
                }

                input = &input[(end + 1)..];
            } else {
                // Static text
                let end = input.find_byte(b'{').unwrap_or(input.len());
                pattern.append_text(&input[..end]);
                input = &input[end..];
            }
        }

        pattern.validate()?;
        Ok(pattern)
    }
}

impl ToString for HyperlinkPattern {
    fn to_string(&self) -> String {
        self.parts.iter().map(|p| p.to_string()).collect()
    }
}

impl Part {
    fn render(
        &self,
        values: &HyperlinkValues,
        output: &mut impl Write,
    ) -> io::Result<()> {
        match self {
            Part::Text(text) => output.write_all(text),
            Part::File => output.write_all(&values.file.0),
            Part::Line => write!(output, "{}", values.line),
            Part::Column => write!(output, "{}", values.column),
        }
    }
}

impl ToString for Part {
    fn to_string(&self) -> String {
        match self {
            Part::Text(text) => String::from_utf8_lossy(text).to_string(),
            Part::File => "{file}".to_string(),
            Part::Line => "{line}".to_string(),
            Part::Column => "{column}".to_string(),
        }
    }
}

impl Display for HyperlinkPatternError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HyperlinkPatternError::InvalidSyntax => {
                write!(f, "invalid hyperlink pattern syntax.")
            }
            HyperlinkPatternError::NoFilePlaceholder => {
                write!(f, "the {{file}} placeholder is required in hyperlink patterns.")
            }
            HyperlinkPatternError::NoLinePlaceholder => {
                write!(f, "the hyperlink pattern contains a {{column}} placeholder, \
                    but no {{line}} placeholder is present.")
            }
            HyperlinkPatternError::InvalidPlaceholder(name) => {
                write!(
                    f,
                    "invalid hyperlink pattern placeholder: '{}'. Choose from: \
                        file, line, column, host.",
                    name
                )
            }
            HyperlinkPatternError::InvalidScheme => {
                write!(f, "the hyperlink pattern must start with a valid URL scheme.")
            }
        }
    }
}

impl Error for HyperlinkPatternError {}

impl<'a> HyperlinkValues<'a> {
    /// Creates a new set of hyperlink values.
    pub fn new(
        file: &'a HyperlinkPath,
        line: Option<u64>,
        column: Option<u64>,
    ) -> Self {
        HyperlinkValues {
            file,
            line: line.unwrap_or(1),
            column: column.unwrap_or(1),
        }
    }
}

impl HyperlinkPath {
    /// Returns a hyperlink path from an OS path.
    #[cfg(unix)]
    pub fn from_path(path: &Path) -> Option<Self> {
        // On Unix, this function returns the absolute file path without the leading slash,
        // as it makes for more natural hyperlink patterns, for instance:
        //   file://{host}/{file}   instead of   file://{host}{file}
        //   vscode://file/{file}   instead of   vscode://file{file}
        // It also allows for the VSCode pattern to be multi-platform.

        let path = path.canonicalize().ok()?;
        let path = path.to_str()?.as_bytes();
        let path = if path.starts_with(b"/") { &path[1..] } else { path };
        Some(Self::encode(path))
    }

    /// Returns a hyperlink path from an OS path.
    #[cfg(windows)]
    pub fn from_path(path: &Path) -> Option<Self> {
        // On Windows, Path::canonicalize returns the result of
        // GetFinalPathNameByHandleW with VOLUME_NAME_DOS,
        // which produces paths such as the following:
        //   \\?\C:\dir\file.txt           (local path)
        //   \\?\UNC\server\dir\file.txt   (network share)
        //
        // The \\?\ prefix comes from VOLUME_NAME_DOS and is constant.
        // It is followed either by the drive letter, or by UNC\
        // (universal naming convention), which denotes a network share.
        //
        // Given that the default URL pattern on Windows is file:///{file}
        // we need to return the following from this function:
        //   C:/dir/file.txt        (local path)
        //   /server/dir/file.txt   (network share)
        //
        // Which produces the following links:
        //   file:///C:/dir/file.txt        (local path)
        //   file:////server/dir/file.txt   (network share)
        //
        // This substitutes the {file} placeholder with the expected value
        // for the most common DOS paths, but on the other hand,
        // network paths start with a single slash, which may be unexpected.
        // It produces correct URLs though.
        //
        // Note that the following URL syntax is also valid for network shares:
        //   file://server/dir/file.txt
        // It is also more consistent with the Unix case, but in order to
        // use it, the pattern would have to be file://{file} and
        // the {file} placeholder would have to be replaced with
        //   /C:/dir/file.txt
        // for local files, which is not ideal, and it is certainly unexpected.
        //
        // Also note that the file://C:/dir/file.txt syntax is not correct,
        // even though it often works in practice.
        //
        // In the end, this choice was confirmed by VSCode, whose pattern
        // is vscode://file/{file}:{line}:{column} and which correctly understands
        // the following URL format for network drives:
        //   vscode://file//server/dir/file.txt:1:1
        // It doesn't parse any other number of slashes in "file//server" as a network path.

        const WIN32_NAMESPACE_PREFIX: &[u8] = br"\\?\";
        const UNC_PREFIX: &[u8] = br"UNC\";

        let path = path.canonicalize().ok()?;
        let mut path = path.to_str()?.as_bytes();

        if path.starts_with(WIN32_NAMESPACE_PREFIX) {
            path = &path[WIN32_NAMESPACE_PREFIX.len()..];

            if path.starts_with(UNC_PREFIX) {
                path = &path[(UNC_PREFIX.len() - 1)..];
            }
        } else {
            return None;
        }

        Some(Self::encode(path))
    }

    fn encode(input: &[u8]) -> HyperlinkPath {
        // The ASCII alphanumeric characters and "-", ".", "_", "~"
        // are unreserved as per section 2.3 of RFC 3986 (URI Syntax).
        // Additionally, RFC 8089 ("file" Scheme), does not specify
        // encoding requirements. Passing non-ASCII characters as-is
        // seems to work best. Other ASCII characters are percent-encoded.

        let mut result = Vec::with_capacity(input.len());

        for &c in input {
            match c {
                b'0'..=b'9'
                | b'A'..=b'Z'
                | b'a'..=b'z'
                | b'/'
                | b':'
                | b'-'
                | b'.'
                | b'_'
                | b'~'
                | 128.. => {
                    result.push(c);
                }
                #[cfg(windows)]
                b'\\' => {
                    result.push(b'/');
                }
                _ => {
                    const HEX: &[u8] = b"0123456789ABCDEF";
                    result.push(b'%');
                    result.push(HEX[(c >> 4) as usize]);
                    result.push(HEX[(c & 0xF) as usize]);
                }
            }
        }

        Self(result)
    }
}

impl Display for HyperlinkPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            std::str::from_utf8(&self.0).unwrap_or("invalid utf-8")
        )
    }
}

/// A simple abstraction over a hyperlink span written to the terminal.
/// This helps tracking whether a hyperlink has been started, and should be ended.
#[derive(Debug, Default)]
pub struct HyperlinkSpan {
    active: bool,
}

impl HyperlinkSpan {
    /// Starts a hyperlink and returns a span which tracks whether it is still in effect.
    pub fn start(
        wtr: &mut impl WriteColor,
        hyperlink: &HyperlinkSpec,
    ) -> io::Result<Self> {
        if wtr.supports_hyperlinks() && hyperlink.uri().is_some() {
            wtr.set_hyperlink(hyperlink)?;
            Ok(HyperlinkSpan { active: true })
        } else {
            Ok(HyperlinkSpan { active: false })
        }
    }

    /// Ends the hyperlink span if it is active.
    pub fn end(&mut self, wtr: &mut impl WriteColor) -> io::Result<()> {
        if self.is_active() {
            wtr.set_hyperlink(&HyperlinkSpec::none())?;
            self.active = false;
        }
        Ok(())
    }

    /// Returns true if there is currently an active hyperlink.
    pub fn is_active(&self) -> bool {
        self.active
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn combine_text_parts() {
        let mut pattern = HyperlinkPattern::new();
        pattern.append_text(b"foo");
        pattern.append_text(b"bar");
        pattern.append_text(b"baz");
        assert_eq!(pattern.to_string(), "foobarbaz");
        assert_eq!(pattern.parts.len(), 1);
    }

    #[test]
    fn handle_alias() {
        assert!(HyperlinkPattern::from_str("file").is_ok());
    }

    #[test]
    fn parse_pattern() {
        let pattern = HyperlinkPattern::from_str(
            "foo://{host}/bar/{file}:{line}:{column}",
        )
        .unwrap();
        assert_eq!(
            pattern.to_string(),
            "foo://{host}/bar/{file}:{line}:{column}".replace(
                "{host}",
                &gethostname::gethostname().to_string_lossy()
            )
        );
        assert_eq!(pattern.parts.len(), 6);
        assert!(pattern.parts.contains(&Part::File));
        assert!(pattern.parts.contains(&Part::Line));
    }

    #[test]
    fn parse_valid() {
        assert!(HyperlinkPattern::from_str("").unwrap().parts.is_empty());
        assert_eq!(
            HyperlinkPattern::from_str("foo://{file}").unwrap().to_string(),
            "foo://{file}"
        );
        assert_eq!(
            HyperlinkPattern::from_str("foo://{file}/bar")
                .unwrap()
                .to_string(),
            "foo://{file}/bar"
        );

        HyperlinkPattern::from_str("f://{file}").unwrap();
        HyperlinkPattern::from_str("f:{file}").unwrap();
        HyperlinkPattern::from_str("f-+.:{file}").unwrap();
        HyperlinkPattern::from_str("f42:{file}").unwrap();
    }

    #[test]
    fn parse_invalid() {
        assert_eq!(
            HyperlinkPattern::from_str("foo://bar").unwrap_err(),
            HyperlinkPatternError::NoFilePlaceholder
        );
        assert_eq!(
            HyperlinkPattern::from_str("foo://{bar}").unwrap_err(),
            HyperlinkPatternError::InvalidPlaceholder("bar".to_string())
        );
        assert_eq!(
            HyperlinkPattern::from_str("foo://{file").unwrap_err(),
            HyperlinkPatternError::InvalidSyntax
        );
        assert_eq!(
            HyperlinkPattern::from_str("foo://{file}:{column}").unwrap_err(),
            HyperlinkPatternError::NoLinePlaceholder
        );
        assert_eq!(
            HyperlinkPattern::from_str("{file}").unwrap_err(),
            HyperlinkPatternError::InvalidScheme
        );
        assert_eq!(
            HyperlinkPattern::from_str(":{file}").unwrap_err(),
            HyperlinkPatternError::InvalidScheme
        );
        assert_eq!(
            HyperlinkPattern::from_str("f*:{file}").unwrap_err(),
            HyperlinkPatternError::InvalidScheme
        );
    }

    #[test]
    fn aliases_are_valid() {
        for (name, definition) in HYPERLINK_PATTERN_ALIASES {
            assert!(
                HyperlinkPattern::from_str(definition).is_ok(),
                "invalid hyperlink alias: {}",
                name
            );
        }
    }

    #[test]
    fn aliases_are_sorted() {
        let mut names = HYPERLINK_PATTERN_ALIASES.iter().map(|(name, _)| name);

        let Some(mut previous_name) = names.next() else { return; };

        for name in names {
            assert!(
                name > previous_name,
                r#""{}" should be sorted before "{}" in `HYPERLINK_PATTERN_ALIASES`"#,
                name,
                previous_name
            );

            previous_name = name;
        }
    }
}
