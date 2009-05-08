package App::Clk::Storage;
use strict;
use warnings;

sub open {
    my ($class, $name) = @_;
    my $storage_class = "App::Clk::Storage::$name";
    eval "require $storage_class";
    return $storage_class->open;
}

1;
