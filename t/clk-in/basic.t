use strict;
use warnings;
use Test::More tests => 9;
use App::Clk::Test;

clk_setup_test({
    fake_time => '2008-03-19T12:34:56Z',
});

# about as simple as it gets
cmd_ok <<'...';
$ ./clk in --output-only
> time: 2008-03-19T12:34:56Z
...

# manually clocking out
cmd_ok <<'...', { at => '2008-03-19T12:34:59Z' };
$ ./clk in --output-only out
> time: 2008-03-19T12:34:59Z
> text: out
...

# longer text and option at the end
cmd_ok <<'...', { at => '2008-03-19T12:35:12Z' },
$ ./clk-in how\'s this for "a message" --output-only
> time: 2008-03-19T12:35:12Z
> text: how's this for a message
...
