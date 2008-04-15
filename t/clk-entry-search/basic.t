use strict;
use warnings;
use Test::More tests => 9;
use App::Clk::Test;

clk_setup_test();

# oops, forgot to provide an entry ID
cmd_ok <<'...';
$ ./clk entry-search
! No entry ID given
? 255
...

# the entry does exist
touch_file('entries/ab/cdef0000000000000000000000000000000000');
cmd_ok <<'...';
$ ./clk entry-search abcdef0000000000000000000000000000000000
> abcdef0000000000000000000000000000000000
...

# the entry does not exist
cmd_ok <<'...';
$ ./clk entry-search 9999999999999999999999999999999999999999
? 1
...

# TODO search for an entry based on a partial entry ID
# TODO search for an entry based on it's relative distance
#      from the end of the timeline.  This is the "-2"
#      naming convention.
# TODO search for all entries within two epoch times
