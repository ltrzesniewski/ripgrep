use crate::util::{Dir, TestCommand, sort_lines};

// This file tests for ripgrep's handling of input sources
// and relations between them.
// See: https://github.com/BurntSushi/ripgrep/issues/3459

// Tests for correct usage

rgtest!(positional_args, |dir: Dir, mut cmd: TestCommand| {
    dir.create("foo", "match");
    dir.create("bar", "match");
    dir.create("baz", "match");
    dir.create("other", "match");
    cmd.arg("match").arg("foo").arg("bar").arg("baz").arg("-l");
    eqnice!(sort_lines("foo\nbar\nbaz\n"), sort_lines(&cmd.stdout()));
});

rgtest!(in_terminated, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\nbar\r\nbaz\n"); // Tests both LF and CRLF
    dir.create("foo", "match");
    dir.create("bar", "match");
    dir.create("baz", "match");
    dir.create("other", "match");
    cmd.arg("match").arg("--in").arg("input").arg("-l");
    eqnice!(sort_lines("foo\nbar\nbaz\n"), sort_lines(&cmd.stdout()));
});

rgtest!(in0_terminated, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\x00bar\x00baz\x00");
    dir.create("foo", "match");
    dir.create("bar", "match");
    dir.create("baz", "match");
    dir.create("other", "match");
    cmd.arg("match").arg("--in0").arg("input").arg("-l");
    eqnice!(sort_lines("foo\nbar\nbaz\n"), sort_lines(&cmd.stdout()));
});

rgtest!(in_unterminated, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\nbar\r\nbaz"); // Tests both LF and CRLF
    dir.create("foo", "match");
    dir.create("bar", "match");
    dir.create("baz", "match");
    dir.create("other", "match");
    cmd.arg("match").arg("--in").arg("input").arg("-l");
    eqnice!(sort_lines("foo\nbar\nbaz\n"), sort_lines(&cmd.stdout()));
});

rgtest!(in0_unterminated, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\x00bar\x00baz");
    dir.create("foo", "match");
    dir.create("bar", "match");
    dir.create("baz", "match");
    dir.create("other", "match");
    cmd.arg("match").arg("--in0").arg("input").arg("-l");
    eqnice!(sort_lines("foo\nbar\nbaz\n"), sort_lines(&cmd.stdout()));
});

// Tests specific to --in

rgtest!(in_with_nul_byte, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\x00bar");
    dir.create("foo", "match");
    dir.create("bar", "match");
    cmd.arg("match").arg("--in").arg("input");
    cmd.assert_exit_code(2);
});

// Tests for handling of non-existing files

rgtest!(in_non_existing_file, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in").arg("does_not_exist");
    cmd.assert_exit_code(2);
});

rgtest!(in0_non_existing_file, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in0").arg("does_not_exist");
    cmd.assert_exit_code(2);
});

rgtest!(in_contains_non_existing_file, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo");
    cmd.arg("match").arg("--in").arg("input");
    cmd.assert_exit_code(2);
});

rgtest!(in0_contains_non_existing_file, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo");
    cmd.arg("match").arg("--in0").arg("input");
    cmd.assert_exit_code(2);
});

// Tests for stdin consumption

rgtest!(arg_stdin_consumed_by_in, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in").arg("-").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(arg_stdin_consumed_by_in0, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in0").arg("-").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(arg_stdin_consumed_by_search, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("-").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(arg_stdin_consumed_by_file, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--file").arg("-").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in_stdin_consumed_by_in, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in").arg("-").arg("--in").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in_stdin_consumed_by_in0, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in0").arg("-").arg("--in").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in_stdin_consumed_by_search, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("-").arg("--in").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in_stdin_consumed_by_file, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--file").arg("-").arg("--in").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in0_stdin_consumed_by_in, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in").arg("-").arg("--in0").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in0_stdin_consumed_by_in0, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--in0").arg("-").arg("--in0").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in0_stdin_consumed_by_search, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("-").arg("--in0").arg("-");
    cmd.assert_exit_code(2);
});

rgtest!(in0_stdin_consumed_by_file, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("match").arg("--file").arg("-").arg("--in0").arg("-");
    cmd.assert_exit_code(2);
});

// Test for input ordering

rgtest!(input_order, |dir: Dir, mut cmd: TestCommand| {
    // Add a few more files than what we want to actually search.
    for i in 1..=12 {
        dir.create(format!("file{}", i), "match");
    }

    dir.create("inputA", "file6\nfile3\n");
    dir.create("inputB", "file1\x00file8\x00");

    cmd.arg("match").arg("--threads").arg("1").arg("-l");

    // Mingle the file order to exclude any potential sorting effect.
    cmd.arg("file7").arg("file5");
    cmd.arg("--in").arg("inputA");
    cmd.arg("file2").arg("file4");
    cmd.arg("--in0").arg("inputB");
    cmd.arg("file10").arg("file9");

    eqnice!(
        "\
file7
file5
file6
file3
file2
file4
file1
file8
file10
file9
",
        &cmd.stdout()
    );
});
