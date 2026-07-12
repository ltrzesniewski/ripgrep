use crate::util::{Dir, TestCommand, sort_lines};

// This file tests for ripgrep's handling of input sources and their relations.
// See: https://github.com/BurntSushi/ripgrep/issues/3459

rgtest!(input_from_in, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\nbar\r\nbaz\nqux");
    dir.create("foo", "match");
    dir.create("bar", "match");
    dir.create("baz", "match");
    dir.create("qux", "match");
    dir.create("other", "match");
    cmd.arg("--in").arg("input").arg("-l").arg("match");
    eqnice!(sort_lines("foo\nbar\nbaz\nqux\n"), sort_lines(&cmd.stdout()));
});

rgtest!(input_from_in_non_existing_file, |_dir: Dir, mut cmd: TestCommand| {
    cmd.arg("--in").arg("does_not_exist").arg("match");
    cmd.assert_exit_code(2);
});

rgtest!(
    input_from_in_contains_non_existing_file,
    |dir: Dir, mut cmd: TestCommand| {
        dir.create("input", "foo");
        dir.create("bar", "match");
        cmd.arg("--in").arg("input").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in_stdin_consumed_by_in,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--in").arg("-").arg("--in").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in_stdin_consumed_by_in0,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--in0").arg("-").arg("--in").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in_stdin_consumed_by_search,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--in").arg("-").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in_stdin_consumed_by_file,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--file").arg("-").arg("--in").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(input_from_in_contains_nul_byte, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\x00bar");
    dir.create("foo", "match");
    dir.create("bar", "match");
    cmd.arg("--in").arg("input").arg("match");
    cmd.assert_exit_code(2);
});

rgtest!(input_from_in0, |dir: Dir, mut cmd: TestCommand| {
    dir.create("input", "foo\x00bar\x00baz");
    dir.create("foo", "match");
    dir.create("bar", "match");
    dir.create("baz", "match");
    dir.create("other", "match");
    cmd.arg("--in0").arg("input").arg("-l").arg("match");
    eqnice!(sort_lines("foo\nbar\nbaz\n"), sort_lines(&cmd.stdout()));
});

rgtest!(
    input_from_in0_non_existing_file,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--in0").arg("does_not_exist").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in0_contains_non_existing_file,
    |dir: Dir, mut cmd: TestCommand| {
        dir.create("input", "foo");
        cmd.arg("--in0").arg("input").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in0_stdin_consumed_by_in,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--in").arg("-").arg("--in0").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in0_stdin_consumed_by_in0,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--in0").arg("-").arg("--in0").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in0_stdin_consumed_by_search,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--in0").arg("-").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);

rgtest!(
    input_from_in0_stdin_consumed_by_file,
    |_dir: Dir, mut cmd: TestCommand| {
        cmd.arg("--file").arg("-").arg("--in0").arg("-").arg("match");
        cmd.assert_exit_code(2);
    }
);
