use strict;
use warnings;
use Test::More tests => 14;
use App::Clk::Test;

clk_setup_test({
    fake_time => '2008-03-19T12:34:56Z',
});

# about as simple as it gets
cmd_ok <<'...';
$ ./clk in --output-only
> time: 2008-03-19T12:34:56Z
> identity: tester@example.org
...

# manually clocking out
cmd_ok <<'...', { at => '2008-03-19T12:34:59Z' };
$ ./clk in --output-only out
> time: 2008-03-19T12:34:59Z
> identity: tester@example.org
> text: out
...

# longer text and option at the end
cmd_ok <<'...', { at => '2008-03-19T12:35:12Z' },
$ ./clk-in how\'s this for "a message" --output-only
> time: 2008-03-19T12:35:12Z
> identity: tester@example.org
> text: how's this for a message
...


# now that we're pretty sure clk-in correctly generates
# entry content, make sure that it can drive clk-store-entry
cmd_ok <<'...', { at => '2008-04-12T06:10:49Z' };
$ ./clk in etc etc etc
> 0966e4c86773d3da8ec8b11ae77f5e7a00f5aaff
...
files_ok <<'...';
entries/09/66e4c86773d3da8ec8b11ae77f5e7a00f5aaff
    time: 2008-04-12T06:10:49Z
    identity: tester@example.org
    text: etc etc etc
timelines/77/d4c7b0bed8729699f21bf194d533f6c6515eac/48005269
    48005269 0966e4c86773d3da8ec8b11ae77f5e7a00f5aaff
...
