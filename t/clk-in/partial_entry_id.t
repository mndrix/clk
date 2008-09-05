use strict;
use warnings;
use Test::More tests => 9;
use App::Clk::Test;

clk_setup_test({
    fake_time => '2008-04-12T06:10:49Z',
});

# create an entry
cmd_ok <<'...';
$ ./clk in etc etc etc
> 95579fcee7ee1a61657a0fc50339e216c851c334
...

# ... and use it as a template
cmd_ok <<'...', { at => '2008-04-12T06:10:53Z' };
$ ./clk in 95579 --output-only
> time: 2008-04-12T06:10:53Z
> text: etc etc etc
...

# clk in with a message that looks like an entry ID but isn't
cmd_ok <<'...', { at => '2008-04-12T06:10:57Z' };
$ ./clk in deadbeef --output-only
> time: 2008-04-12T06:10:57Z
> text: deadbeef
...


# TODO test hooks when using entry IDs to clk in
# TODO      currently, hooks disable the ability to clk in
# TODO      with a partial entry ID
