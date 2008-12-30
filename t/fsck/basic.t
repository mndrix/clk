use strict;
use warnings;
use Test::More tests => 6;
use App::Clk::Test;
use App::Clk::Util qw( hashed_path );

clk_setup_test({ make_data => 1 });

# the database should be OK when we start
cmd_ok <<'...';
$ ./clk fsck
...


# wreak all sorts of havoc and make sure it's noticed

## corrupt a single entry
my $path = hashed_path(
    '489931bdd0314f6bd84fe735326a9be8fe8c721c',
    '%r/entries/%h',
);
open my $fh, '>', $path;
print $fh "uh oh: this is not what we started with\n";
close $fh;

# delete an entry
$path = hashed_path(
    '053b5c718fb4935dbea789e49600e9abe043a1ea',
    '%r/entries/%h',
);
unlink $path;

# mangle the timeline
$path = (glob "$ENV{CLK_ROOT}/timelines/*/*/*")[0];
open $fh, '>>', $path;
print $fh "99999999 abcdefghijklmnopqrstuvwxyz\n";
close $fh;

cmd_ok <<'...';
$ ./clk fsck
! Entry 489931bdd0314f6bd84fe735326a9be8fe8c721c is corrupt (SHA1 mismatch)
! Entry 053b5c718fb4935dbea789e49600e9abe043a1ea does not exist in the filesystem
! Entry abcdefghijklmnopqrstuvwxyz is not a valid SHA-1 hash
...
