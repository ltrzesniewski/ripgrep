use bstr::ByteSlice;
use std::error::Error;
use std::fmt::Display;
use std::io;
use std::io::Write;
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HyperlinkValues<'a> {
    file: &'a [u8],
    line: u64,
    column: u64,
}

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
            Part::File => output.write_all(values.file),
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
        file: &'a [u8],
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
}
