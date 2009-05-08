package App::Clk;
use strict;
use warnings;

sub storage {
    require App::Clk::Storage;
    return App::Clk::Storage->open('SQLite');
}

1;
