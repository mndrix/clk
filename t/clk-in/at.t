use strict;
use warnings;
use Test::More tests => 12;
use App::Clk::Test;

clk_setup_test();

# common error conditions
cmd_ok <<'...';
$ ./clk in --at
! --at requires an argument
? 255
...
cmd_ok <<'...';
$ ./clk in --at jjjjjjjjj
! Invalid time format for --at: jjjjjjjjj
? 255
...

# using epoch seconds
cmd_ok <<'...';
$ ./clk in --at 1208030762 --output-only
> time: 2008-04-12T20:06:02Z
...
cmd_ok <<'...';
$ ./clk in  --output-only --at 1208030762
> time: 2008-04-12T20:06:02Z
...

# TODO --at with no arguments
# TODO --at with invalid arguments

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
