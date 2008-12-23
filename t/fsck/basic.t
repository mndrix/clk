use strict;
use warnings;
use Test::More tests => 4;

TODO: {
    local $TODO = 'Implement these';
    ok( 0, 'Everything checks out correctly' );
    ok( 0, 'A corrupted entry' );
    ok( 0, 'A missing entry' );
    ok( 0, 'A malformed SHA1 in the timeline' );
};
