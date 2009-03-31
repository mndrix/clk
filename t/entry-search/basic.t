use strict;
use warnings;
use Test::More tests => 36;
use App::Clk::Test;
use App::Clk::Util qw( entry_search );

clk_setup_test({ make_data => 1 });

# giving no entry ID matches all entries
{
    my $entries = entry_search();
    my @expect = (
        [ '489931bdd0314f6bd84fe735326a9be8fe8c721c', 1223350310 ],
        [ '053b5c718fb4935dbea789e49600e9abe043a1ea', 1223350320 ],
        [ '241af75967962f66faad9df1d4bf2780c6da91c5', 1223350430 ],
        [ '8d9fd3f3f02147a205a789fd23513fb53ae5fb50', 1223350439 ],
        [ 'cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98', 1223350566 ],
        [ '0422f139d6becb2a1451b3f2228f4bf633098900', 1223350590 ],
        [ 'dbe3e01151ad7734842e353b9fea5668f1c6e75f', 1223436966 ],
    );
    while ( my $entry = $entries->next ) {
        my ( $id, $time ) = @{ shift @expect };
        isa_ok( $entry, 'App::Clk::Entry' );
        is( $entry->id, $id );
        is( $entry->time, $time );
    }
}

# more specific searches work too
{
    my $entries = entry_search({ tail => 2 });
    my @expect = (
        [ '0422f139d6becb2a1451b3f2228f4bf633098900', 1223350590 ],
        [ 'dbe3e01151ad7734842e353b9fea5668f1c6e75f', 1223436966 ],
    );
    while ( my $entry = $entries->next ) {
        my ( $id, $time ) = @{ shift @expect };
        isa_ok( $entry, 'App::Clk::Entry' );
        is( $entry->id, $id );
        is( $entry->time, $time );
    }
}

# command line options with multiple arguments
{
    my $entries = entry_search({ between => [ 1223350560, 1223436970 ] });
    my @expect = (
        [ 'cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98', 1223350566 ],
        [ '0422f139d6becb2a1451b3f2228f4bf633098900', 1223350590 ],
        [ 'dbe3e01151ad7734842e353b9fea5668f1c6e75f', 1223436966 ],
    );
    while ( my $entry = $entries->next ) {
        my ( $id, $time ) = @{ shift @expect };
        isa_ok( $entry, 'App::Clk::Entry' );
        is( $entry->id, $id );
        is( $entry->time, $time );
    }
}
