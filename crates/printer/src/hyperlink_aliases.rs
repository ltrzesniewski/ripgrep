/// Aliases to well-known hyperlink schemes.
///
/// These need to be sorted by name.
pub const HYPERLINK_PATTERN_ALIASES: &[(&str, &str)] = &[
    #[cfg(unix)]
    ("file", "file://{host}/{file}"),
    #[cfg(windows)]
    ("file", "file:///{file}"),
    ("kitty", "file://{host}/{file}#{line}"),
    ("vscode", "vscode://file/{file}:{line}:{column}"),
];
