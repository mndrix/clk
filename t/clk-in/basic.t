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


# now that we're pretty sure clk-in correctly generates
# entry content, make sure that it can drive clk-store-entry
cmd_ok <<'...', { at => '2008-04-12T06:10:49Z' };
$ ./clk in etc etc etc
> 95579fcee7ee1a61657a0fc50339e216c851c334
...
files_ok <<'...';
entries/95/579fcee7ee1a61657a0fc50339e216c851c334
    time: 2008-04-12T06:10:49Z
    text: etc etc etc
timelines/48005269
    48005269 95579fcee7ee1a61657a0fc50339e216c851c334
...
