use strict;
use warnings;
use Test::More tests => 12;
use App::Clk::Test;

clk_setup_test({
    fake_time => '2008-03-19T12:34:56Z',
});

# cat an entry
cmd_ok <<'...';
$ ./clk in cat in the hat
> a7e7ebb994f3c02e16bb62e441b13adecd4eaa5c
...
cmd_ok <<'...';
$ ./clk cat-entry a7e7ebb994f3c02e16bb62e441b13adecd4eaa5c
> time: 2008-03-19T12:34:56Z
> identity: tester@example.org
> text: cat in the hat
...

# try an entry that doesn't exist
cmd_ok <<'...';
$ ./clk cat-entry 7777777777777777777777777777777777777777
! No such entry
? 1
...

# try a usage error
cmd_ok <<'...';
$ ./clk cat-entry
! You must specify an entry ID
? 255
...

