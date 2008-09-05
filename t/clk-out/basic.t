use strict;
use warnings;
use Test::More tests => 3;
use App::Clk::Test;

clk_setup_test({
    fake_time => '2008-03-19T12:34:56Z',
});

# about as simple as it gets
cmd_ok <<'...';
$ ./clk out --output-only
> time: 2008-03-19T12:34:56Z
> text: out
...
