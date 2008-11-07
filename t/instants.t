use strict;
use warnings;
use Test::More;
use App::Clk::Test;
use App::Clk::Util qw( resolve_timespec );

my @phrases = (
    'now' => [
        [
            '2007-10-08T21:33:40',   # base
            '2007-10-08T21:33:40',   # expected time
        ],
    ],
    '1226099273' => [
        [
            '2007-10-08T21:33:40',   # base
            '2008-11-07T16:07:53',   # expected time
        ],
        [
            '2008-12-13T01:30:07',   # base
            '2008-11-07T16:07:53',   # expected time
        ],
    ],
    '5m' => [
        [
            '2007-10-08T21:33:40',   # base
            '2007-10-08T21:28:40',   # expected time
        ],
        [
            '2008-12-13T01:30:07',   # base
            '2008-12-13T01:25:07',   # expected time
        ],
        [
            '2007-01-01T00:04:09',   # base
            '2006-12-31T23:59:09',   # expected time
        ],
    ],
);

plan tests => 8;

while ( my ($phrase, $tests) = splice(@phrases, 0, 2) ) {
    for my $test (@$tests) {
        my ($base, $expected) = @$test;
        fake_time($base);

        my $got = resolve_timespec($phrase);
        is( iso($got), $expected, "$phrase ok" );
    }
}

# test for some failure conditions
{
    my $scalar = eval { resolve_timespec('invalid instant') };
    like( $@, qr/^Unknown instant description:/, 'invalid instant exception' );
    is( $scalar, undef, 'invalid instant in scalar context' );
}
