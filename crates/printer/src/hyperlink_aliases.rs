/// Aliases to well-known hyperlink schemes.
///
/// These need to be sorted by name.
pub const HYPERLINK_PATTERN_ALIASES: &[(&str, &str)] = &[
    #[cfg(unix)]
    ("file", "file://{host}/{file}"),
    #[cfg(windows)]
    ("file", "file:///{file}"),
    ("kitty", "file://{host}/{file}#{line}"),
    ("none", ""),
    // https://code.visualstudio.com/docs/editor/command-line#_opening-vs-code-with-urls
    ("vscode", "vscode://file/{file}:{line}:{column}"),
    ("vscode-insiders", "vscode-insiders://file/{file}:{line}:{column}"),
];
