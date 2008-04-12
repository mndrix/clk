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


# TODO be able to "clk out"
# TODO be able to "clk in" with a simple message
# TODO be able to "clk in" at a particular time in the past
#      using some kind of --at option.  Testing this
#      will require some additions to cmd_ok so that it
#      can make believe that it's running at a particular time.
# TODO allow for inference hooks so that people can customize
#      the way that an entry is generated based the "in"
#      commands arguments.  Perhaps this is only a single hook.
#      If someone wants to have multiple layers of inference
#      hooks, they can have the one true hook call out to those
#      smaller hooks.