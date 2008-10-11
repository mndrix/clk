use strict;
use warnings;
use Test::More;
use App::Clk::Test;
use App::Clk::Util qw( resolve_period );
use POSIX qw( strftime );

my @phrases = (
    'today' => [
        [
            '2007-10-08T21:33:40',   # base
            '2007-10-08T00:00:00',   # expected from
            '2007-10-08T23:59:59',   # expected to
        ],
    ],
#   'yesterday' => [
#       [
#           '2007-10-08T21:33:40',   # base
#           '2007-10-07T00:00:00',   # expected from
#           '2007-10-07T23:59:59',   # expected to
#       ],
#   ],
#   'yesterday' => [
#       [
#           '2008-01-01T22:44:18',   # base
#           '2007-12-31T00:00:00',   # expected from
#           '2007-12-31T23:59:59',   # expected to
#       ],
#   ],

#   # names of the days of the week interpreted to be in the past
#   'monday' => [
#       [
#           '2008-01-12T13:00:27',   # base
#           '2008-01-07T00:00:00',   # expected from
#           '2008-01-07T23:59:59',   # expected to
#       ],
#   ],
#   'tuesday' => [
#       [
#           '2008-01-12T13:00:27',   # base
#           '2008-01-08T00:00:00',   # expected from
#           '2008-01-08T23:59:59',   # expected to
#       ],
#   ],
#   'wednesday' => [
#       [
#           '2008-01-12T13:00:27',   # base
#           '2008-01-09T00:00:00',   # expected from
#           '2008-01-09T23:59:59',   # expected to
#       ],
#   ],
#   'thursday' => [
#       [
#           '2008-01-12T13:00:27',   # base
#           '2008-01-10T00:00:00',   # expected from
#           '2008-01-10T23:59:59',   # expected to
#       ],
#   ],
#   'friday' => [
#       [
#           '2008-01-12T13:00:27',   # base
#           '2008-01-11T00:00:00',   # expected from
#           '2008-01-11T23:59:59',   # expected to
#       ],
#   ],
#   'saturday' => [
#       [
#           '2008-01-12T13:00:27',   # base
#           '2008-01-05T00:00:00',   # expected from
#           '2008-01-05T23:59:59',   # expected to
#       ],
#   ],
#   'sunday' => [
#       [
#           '2008-01-12T13:00:27',   # base
#           '2008-01-06T00:00:00',   # expected from
#           '2008-01-06T23:59:59',   # expected to
#       ],
#   ],

#   # week-size date ranges
#   'this week' => [
#       [
#           '2008-09-02T22:21:05', # base
#           '2008-09-01T00:00:00', # expected from
#           '2008-09-07T23:59:59', # expected to
#       ],
#   ],
#   'this week' => [  # crossing a month boundary
#       [
#           '2008-10-03T12:34:56', # base
#           '2008-09-29T00:00:00', # expected from
#           '2008-10-05T23:59:59', # expected to
#       ],
#   ],
#   'this week' => [  # includes a DST change
#       [
#           '2007-11-02T02:00:00', # base
#           '2007-10-29T00:00:00', # expected from
#           '2007-11-04T23:59:59', # expected to
#       ],
#   ],
);

plan tests => 2 + 2 * ( @phrases / 2 );

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


sub iso {
    my ($time) = @_;
    return q{} if not $time;
    return strftime( "%Y-%m-%dT%H:%M:%S", localtime($time) );
}
