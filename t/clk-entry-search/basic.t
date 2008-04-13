use strict;
use warnings;
use Test::More tests => 1;
use App::Clk::Test;

TODO: {
    local $TODO = 'search by full entry ID not working yet';
    ok(0);

# marking these tests TODO isn't working, oh well
=pod
    touch_file('entries/ab/cdef0000000000000000000000000000000000');

    # the entry does exist
    cmd_ok <<'...';
$ ./clk entry-search abcdef0000000000000000000000000000000000
> abcdef0000000000000000000000000000000000
...

    # the entry does not exist
    cmd_ok <<'...';
$ ./clk entry-search 9999999999999999999999999999999999999999
? 1
...
=cut

}

# TODO search for an entry based on a complete entry ID
# TODO search for an entry based on a partial entry ID
# TODO search for an entry based on it's relative distance
#      from the end of the timeline.  This is the "-2"
#      naming convention.
# TODO search for all entries within two epoch times
