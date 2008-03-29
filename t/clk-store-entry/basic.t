use strict;
use warnings;
use Test::More tests => 30;
use App::Clk::Test;
use File::Path qw( rmtree );

clk_setup_test();

# make sure the directory structure is created and populated
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T01:31:12Z
> cd249e834ebf56ed13a8a3c54985d8ad471167da
...
files_ok <<'...';
entries/cd/249e834ebf56ed13a8a3c54985d8ad471167da
    time: 2008-03-29T01:31:12Z
timelines/47ed9be0
    47ed9be0 cd249e834ebf56ed13a8a3c54985d8ad471167da
...

# new entry appended to the existing timeline
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T01:57:54Z
> 76f5c0767b7a91bc2e9ee4e71e858ff531892a4c
...
files_ok <<'...';
entries/76/f5c0767b7a91bc2e9ee4e71e858ff531892a4c
    time: 2008-03-29T01:57:54Z
timelines/47ed9be0
    47ed9be0 cd249e834ebf56ed13a8a3c54985d8ad471167da
    47eda222 76f5c0767b7a91bc2e9ee4e71e858ff531892a4c
...

# make an entry earlier than the first entry (creates a second timeline)
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-01-01T01:01:01Z
> b05310c0a2793d326bbc0f540a4fa1e62c81e78a
...
files_ok <<'...';
entries/b0/5310c0a2793d326bbc0f540a4fa1e62c81e78a
    time: 2008-01-01T01:01:01Z
timelines/477990cd
    477990cd b05310c0a2793d326bbc0f540a4fa1e62c81e78a
...

# make an entry later than them all
# the new entry is recorded in the timeline whose earliest entry
# is closest to the new entry yet still has room on the end
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T02:59:53Z
> 51981903bf0cdcd5f52aafbebf18c745453c7912
...
files_ok <<'...';
entries/51/981903bf0cdcd5f52aafbebf18c745453c7912
    time: 2008-03-29T02:59:53Z
timelines/47ed9be0
    47ed9be0 cd249e834ebf56ed13a8a3c54985d8ad471167da
    47eda222 76f5c0767b7a91bc2e9ee4e71e858ff531892a4c
    47edb0a9 51981903bf0cdcd5f52aafbebf18c745453c7912
...

# make an entry in the middle of the entries in the newest timeline.
# this requires that the new entry be placed in an older timeline
# to guarantee that each timeline is chronologically sorted
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T01:31:13Z
> 491cc24025b2f6146efc092f40992e36632b5973
...
files_ok <<'...';
entries/49/1cc24025b2f6146efc092f40992e36632b5973
    time: 2008-03-29T01:31:13Z
timelines/477990cd
    477990cd b05310c0a2793d326bbc0f540a4fa1e62c81e78a
    47ed9be1 491cc24025b2f6146efc092f40992e36632b5973
...

# an entry may be more complicated and it's still handled correctly
cmd_ok <<'...';
$ ./clk store-entry
< foo: bar
< stuff: this one is a bit longer
< time: 2008-03-29T03:50:15Z
> e6d482a214fbaefebc72ccb70161c18c5419d199
...
files_ok <<'...'
entries/e6/d482a214fbaefebc72ccb70161c18c5419d199
    foo: bar
    stuff: this one is a bit longer
    time: 2008-03-29T03:50:15Z
timelines/47ed9be0
    47ed9be0 cd249e834ebf56ed13a8a3c54985d8ad471167da
    47eda222 76f5c0767b7a91bc2e9ee4e71e858ff531892a4c
    47edb0a9 51981903bf0cdcd5f52aafbebf18c745453c7912
    47edbc77 e6d482a214fbaefebc72ccb70161c18c5419d199
...
