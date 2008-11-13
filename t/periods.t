use strict;
use warnings;
use Test::More;
use App::Clk::Test;
use App::Clk::Util qw( resolve_period );

my @phrases = (
    'today' => [
        [
            '2007-10-08T21:33:40',   # base
            '2007-10-08T00:00:00',   # expected from
            '2007-10-08T23:59:59',   # expected to
        ],
    ],
    'yesterday' => [
        [
            '2007-10-08T21:33:40',   # base
            '2007-10-07T00:00:00',   # expected from
            '2007-10-07T23:59:59',   # expected to
        ],
        [
            '2008-01-01T22:44:18',   # base
            '2007-12-31T00:00:00',   # expected from
            '2007-12-31T23:59:59',   # expected to
        ],
    ],

    # names of the days of the week interpreted to be in the past
    'monday' => [
        [
            '2008-01-12T13:00:27',   # base
            '2008-01-07T00:00:00',   # expected from
            '2008-01-07T23:59:59',   # expected to
        ],
    ],
    'tuesday' => [
        [
            '2008-01-12T13:00:27',   # base
            '2008-01-08T00:00:00',   # expected from
            '2008-01-08T23:59:59',   # expected to
        ],
    ],
    'wednesday' => [
        [
            '2008-01-12T13:00:27',   # base
            '2008-01-09T00:00:00',   # expected from
            '2008-01-09T23:59:59',   # expected to
        ],
    ],
    'thursday' => [
        [
            '2008-01-12T13:00:27',   # base
            '2008-01-10T00:00:00',   # expected from
            '2008-01-10T23:59:59',   # expected to
        ],
    ],
    'friday' => [
        [
            '2008-01-12T13:00:27',   # base
            '2008-01-11T00:00:00',   # expected from
            '2008-01-11T23:59:59',   # expected to
        ],
    ],
    'saturday' => [
        [
            '2008-01-12T13:00:27',   # base
            '2008-01-05T00:00:00',   # expected from
            '2008-01-05T23:59:59',   # expected to
        ],
    ],
    'sunday' => [
        [
            '2008-01-12T13:00:27',   # base
            '2008-01-06T00:00:00',   # expected from
            '2008-01-06T23:59:59',   # expected to
        ],
    ],

    # week-size date ranges
    'this week' => [
        [
            '2008-09-02T22:21:05', # base
            '2008-09-01T00:00:00', # expected from
            '2008-09-07T23:59:59', # expected to
        ],
        [  # crossing a month boundary
            '2008-10-03T12:34:56', # base
            '2008-09-29T00:00:00', # expected from
            '2008-10-05T23:59:59', # expected to
        ],
        [  # includes a DST change
            '2007-11-02T02:00:00', # base
            '2007-10-29T00:00:00', # expected from
            '2007-11-04T23:59:59', # expected to
        ],
    ],
    'last week' => [
        [
            '2006-12-28T21:33:40',   # base
            '2006-12-18T00:00:00',   # expected from
            '2006-12-24T23:59:59',   # expected to
        ],
        [
            '2007-01-19T10:07:22',   # base
            '2007-01-08T00:00:00',   # expected from
            '2007-01-14T23:59:59',   # expected to
        ],
    ],

    # month sized date ranges
    'this month' => [
        [
            '2006-12-28T21:33:40',  # base
            '2006-12-01T00:00:00',   # expected from
            '2006-12-31T23:59:59',   # expected to
        ],
        [
            '2007-01-19T10:07:22',   # base
            '2007-01-01T00:00:00',   # expected from
            '2007-01-31T23:59:59',   # expected to
        ],
    ],
    'last month' => [
        [
            '2006-12-28T21:33:40',   # base
            '2006-11-01T00:00:00',   # expected from
            '2006-11-30T23:59:59',   # expected to
        ],
        [
            '2007-01-19T10:07:22',   # base
            '2006-12-01T00:00:00',   # expected from
            '2006-12-31T23:59:59',   # expected to
        ],
        [   # be careful of March 1st because February is so short
            '2008-03-01T00:01:02',   # base
            '2008-02-01T00:00:00',   # expected from
            '2008-02-29T23:59:59',   # expected to
        ],
    ],

    # year sized date ranges
    'this year' => [
        [
            '2006-12-28T21:33:40',  # base
            '2006-01-01T00:00:00',   # expected from
            '2006-12-31T23:59:59',   # expected to
        ],
        [
            '2007-01-19T10:07:22',   # base
            '2007-01-01T00:00:00',   # expected from
            '2007-12-31T23:59:59',   # expected to
        ],
    ],
    'last year' => [
        [
            '2006-12-28T21:33:40',  # base
            '2005-01-01T00:00:00',   # expected from
            '2005-12-31T23:59:59',   # expected to
        ],
        [
            '2007-01-19T10:07:22',   # base
            '2006-01-01T00:00:00',   # expected from
            '2006-12-31T23:59:59',   # expected to
        ],
    ],

    # unix eternity
    'ever' => [
        [
            '2008-11-11T18:59:43',   # base
            '1969-12-31T17:00:00',   # expected from
            '2038-01-18T20:14:07',   # expected to
        ],
    ],
);

plan tests => 52;

while ( my ($phrase, $tests) = splice(@phrases, 0, 2) ) {
    for my $test (@$tests) {
        my ($base, $right_from, $right_to) = @$test;
        fake_time($base);

        my ( $from, $to ) = resolve_period($phrase);
        is( iso($from), $right_from, "$phrase 'from' ok" );
        is( iso($to), $right_to, "$phrase 'to' ok" );
    }
}

# test for some failure conditions
{
    my $scalar = eval { resolve_period('invalid period description') };
    like( $@, qr/^Unknown period description:/, 'invalid period exception' );
    is( $scalar, undef, 'invalid period in scalar context' );
}
