use strict;
use warnings;
use Test::More tests => 4;
use App::Clk::Util qw( hashed_path );
use FindBin;

# simple hashing
my $sha1 = '0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33';    # "foo"
is hashed_path( $sha1, 'path/%h' ),
  'path/0b/eec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33';
is hashed_path( $sha1, 'path/%h.txt' ),
  'path/0b/eec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33.txt';

# including the clk root
$ENV{CLK_ROOT} = $FindBin::Bin;
is hashed_path( $sha1, '%r/stuff' ), "$FindBin::Bin/stuff";
is hashed_path( $sha1, '%r/path/%h' ),
  "$FindBin::Bin/path/0b/eec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33";
