use strict;
use warnings;
use Test::More tests => 30;
use App::Clk::Test;

clk_setup_test();

# make sure the directory structure is created and populated
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T01:31:12Z
< identity: store-entry@example.org
> fe499be73ad5fcae9867892f45526a1d3609911a
...
files_ok <<'...';
entries/fe/499be73ad5fcae9867892f45526a1d3609911a
    time: 2008-03-29T01:31:12Z
    identity: store-entry@example.org
timelines/b2/1de7a8112dc5744911a37c1a644c2f7014e537/47ed9be0
    47ed9be0 fe499be73ad5fcae9867892f45526a1d3609911a
...

# new entry appended to the existing timeline
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T01:57:54Z
< identity: store-entry@example.org
> 07f34501b44900ed2f2bd2d502438c1a72f009bc
...
files_ok <<'...';
entries/07/f34501b44900ed2f2bd2d502438c1a72f009bc
    time: 2008-03-29T01:57:54Z
    identity: store-entry@example.org
timelines/b2/1de7a8112dc5744911a37c1a644c2f7014e537/47ed9be0
    47ed9be0 fe499be73ad5fcae9867892f45526a1d3609911a
    47eda222 07f34501b44900ed2f2bd2d502438c1a72f009bc
...

# make an entry earlier than the first entry (creates a second timeline)
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-01-01T01:01:01Z
< identity: store-entry@example.org
> 49ae0e61342cb423a7e7a23fae8eb1594ce8f55a
...
files_ok <<'...';
entries/49/ae0e61342cb423a7e7a23fae8eb1594ce8f55a
    time: 2008-01-01T01:01:01Z
    identity: store-entry@example.org
timelines/b2/1de7a8112dc5744911a37c1a644c2f7014e537/477990cd
    477990cd 49ae0e61342cb423a7e7a23fae8eb1594ce8f55a
...

# make an entry later than them all
# the new entry is recorded in the timeline whose earliest entry
# is closest to the new entry yet still has room on the end
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T02:59:53Z
< identity: store-entry@example.org
> 04835c73977fc2680c24c4ad438cb87f5f3475c9
...
files_ok <<'...';
entries/04/835c73977fc2680c24c4ad438cb87f5f3475c9
    time: 2008-03-29T02:59:53Z
    identity: store-entry@example.org
timelines/b2/1de7a8112dc5744911a37c1a644c2f7014e537/47ed9be0
    47ed9be0 fe499be73ad5fcae9867892f45526a1d3609911a
    47eda222 07f34501b44900ed2f2bd2d502438c1a72f009bc
    47edb0a9 04835c73977fc2680c24c4ad438cb87f5f3475c9
...

# make an entry in the middle of the entries in the newest timeline.
# this requires that the new entry be placed in an older timeline
# to guarantee that each timeline is chronologically sorted
cmd_ok <<'...';
$ ./clk store-entry
< time: 2008-03-29T01:31:13Z
< identity: store-entry@example.org
> b4cddde2d2dff5ee0a0fa36bf6956c45102f7195
...
files_ok <<'...';
entries/b4/cddde2d2dff5ee0a0fa36bf6956c45102f7195
    time: 2008-03-29T01:31:13Z
    identity: store-entry@example.org
timelines/b2/1de7a8112dc5744911a37c1a644c2f7014e537/477990cd
    477990cd 49ae0e61342cb423a7e7a23fae8eb1594ce8f55a
    47ed9be1 b4cddde2d2dff5ee0a0fa36bf6956c45102f7195
...

# an entry may be more complicated and it's still handled correctly
cmd_ok <<'...';
$ ./clk store-entry
< foo: bar
< stuff: this one is a bit longer
< time: 2008-03-29T03:50:15Z
< identity: store-entry@example.org
> cb82c0e7c36d743f6d3c8070f9620c10f77a203e
...
files_ok <<'...'
entries/cb/82c0e7c36d743f6d3c8070f9620c10f77a203e
    foo: bar
    stuff: this one is a bit longer
    time: 2008-03-29T03:50:15Z
    identity: store-entry@example.org
timelines/b2/1de7a8112dc5744911a37c1a644c2f7014e537/47ed9be0
    47ed9be0 fe499be73ad5fcae9867892f45526a1d3609911a
    47eda222 07f34501b44900ed2f2bd2d502438c1a72f009bc
    47edb0a9 04835c73977fc2680c24c4ad438cb87f5f3475c9
    47edbc77 cb82c0e7c36d743f6d3c8070f9620c10f77a203e
...
