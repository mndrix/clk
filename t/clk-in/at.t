use strict;
use warnings;
use Test::More tests => 1;

TODO: {
    local $TODO = 'implement some real tests';
    ok(0);
}

# TODO be able to "clk in" at a particular time in the past
#      using some kind of --at option.

# The most fundamental argument for --at is a time in epoch
# seconds.  All other argument formats eventually resolve
# back to that one.  Here are some formats that ought to be
# supported:
#
#   --at 1207981288             epoch seconds --> 2008-04-12T06:21:28Z
#   --at 5m                     5 minutes ago
#   --at 30s                    30 seconds ago
#   --at 1h                     1 hour ago
#   --at 12:47                  the closest 12:47 (local time) in the past
