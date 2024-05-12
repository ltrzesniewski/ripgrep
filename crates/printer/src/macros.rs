/// Like assert_eq, but nicer output for long strings.
#[cfg(test)]
#[macro_export]
macro_rules! assert_eq_printed {
    ($expected:expr, $got:expr) => {
        let expected = &*$expected;
        let got = &*$got;
        if expected != got {
            panic!("
printed outputs differ!

expected:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

got:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
", expected.to_string().replace("\r\n", "<CRLF>").replace("\n", "<LF>").replace("LF>", "LF>\n"),
            got.to_string().replace("\r\n", "<CRLF>").replace("\n", "<LF>").replace("LF>", "LF>\n"));
        }
    }
}
