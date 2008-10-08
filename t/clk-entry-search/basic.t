use strict;
use warnings;
use Test::More tests => 36;
use App::Clk::Test;

clk_setup_test();

# oops, forgot to provide an entry ID
cmd_ok <<'...';
$ ./clk entry-search
! No entry ID given
? 255
...

# searching by full entry ID works for existing entries
touch_file('entries/ab/cdef0000000000000000000000000000000000');
cmd_ok <<'...';
$ ./clk entry-search abcdef0000000000000000000000000000000000
> abcdef0000000000000000000000000000000000
...

# ... and missing entries
cmd_ok <<'...';
$ ./clk entry-search 9999999999999999999999999999999999999999
? 1
...


# search for an entry based on a partial entry ID
cmd_ok <<'...';
$ ./clk entry-search ab
! A partial entry ID must be at least 3 characters long
? 255
...
cmd_ok <<'...';
$ ./clk entry-search abc
> abcdef0000000000000000000000000000000000
...
cmd_ok <<'...';
$ ./clk entry-search abcd
> abcdef0000000000000000000000000000000000
...
cmd_ok <<'...';
$ ./clk entry-search abcdef
> abcdef0000000000000000000000000000000000
...
cmd_ok <<'...';
$ ./clk entry-search 999
? 1
...

# create an entry that's partially ambiguous
touch_file('entries/ab/cdefghi0000000000000000000000000000000');
cmd_ok <<'...';
$ ./clk entry-search abc
! The partial entry ID 'abc' is ambiguous
? 2
...
cmd_ok <<'...';
! The partial entry ID 'abcd' is ambiguous
$ ./clk entry-search abcd
? 2
...
cmd_ok <<'...';
! The partial entry ID 'abcdef' is ambiguous
$ ./clk entry-search abcdef
? 2
...
cmd_ok <<'...';
$ ./clk entry-search abcdefg
> abcdefghi0000000000000000000000000000000
...


# TODO search for an entry based on it's relative distance
#      from the end of the timeline.  This is the "-2"
#      naming convention.
