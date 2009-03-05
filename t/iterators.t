use strict;
use warnings;
use Test::More tests => 17;
use App::Clk::Util qw(
    iterator
    iterator_file_lines
    iterator_merge
    iterator_sorted_merge
);
use File::Temp qw( tempfile );

# a simple iterator
{
    my $i = 0;
    my $it = iterator( sub { $i++ } );
    is $it->peek,      0, 'first peek';
    is $it->peek,      0, 'second peek';
    is $it->next,      0, 'first next';
    is $it->next($it), 1, 'next again';
    is_deeply [ $it->take(5) ], [ 2 .. 6 ], 'the next five';
}

# combining two iterators
{
    my $i = 0;
    my $even = iterator( sub { $i+=2; $i-2 } );
    is $even->next, 0, 'even 1';
    is $even->next, 2, 'even 2';
    is $even->next, 4, 'even 3';

    my $j = 1;
    my $odd = iterator( sub { $j+=2; $j-2 } );
    is $odd->next, 1, 'odd 1';
    is $odd->next, 3, 'odd 2';
    is $odd->next, 5, 'odd 3';

    my $it = iterator_merge(
        sub {
            my $choices = shift;
            return 0 if $choices->[0] < $choices->[1];
            return 1;
        },
        $even, $odd
    );

    is $it->peek, 6, 'merge: peek';
    is $it->next, 6, 'merge: next';
    is_deeply [ $it->take(4) ], [ 7 .. 10 ], '7 through 10 interleaved';
}

# sorted merge and iterators that terminate early
{
    my @consonants = qw( b c d f g h j k l m n p q r s t v w x z );
    my $c    = iterator( sub { shift @consonants } );

    my @vowels = qw( a e i o u );
    my $v    = iterator( sub { shift @vowels } );

    my @odd = qw( y );
    my $o   = iterator( sub { shift @odd } );

    my $it = iterator_sorted_merge( sub { $_[0] cmp $_[1] }, $c, $v, $o );
    is_deeply [ $it->all ], [ 'a' .. 'z' ], 'alphabet';
}

# iterating the lines of a file forwards
{
    my ($fh, $filename) = tempfile();
    print $fh "$_\n" for 1 .. 10;
    close $fh;
    my $forwards = iterator_file_lines( $filename, 'forwards' );
    is_deeply [ $forwards->all ], [ 1 .. 10 ], 'forwards';
    my $backwards = iterator_file_lines( $filename, 'backwards' );
    is_deeply [ $backwards->all ], [ reverse( 1 .. 10 ) ], 'backwards';
}
