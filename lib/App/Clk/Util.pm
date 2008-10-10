package App::Clk::Util;
use strict;
use warnings;

sub import {
    my (@sub_names) = @_;

    my $pkg = caller();
    for my $sub_name (@sub_names) {
        no strict 'refs';
        *{$pkg . '::' . $sub_name} = \&$sub_name;
    }

    return;
}

sub clk_root {
    my $root = $ENV{CLK_ROOT}
        or die "Please set CLK_ROOT before using clk\n";
    die "CLK_ROOT=$root is not a directory\n" if not -d $root;
    return $root;
}

# given a string representation of an instant in time, it returns
# that instant as the number of seconds since the epoch.  Supported
# instant formats are:
#
#   * a number of seconds since the epoch
#   * a number followed by the letter "m" indicates a number of
#     minutes in the past
sub resolve_timespec {
    my ($string) = @_;
    my $time = $ENV{CLK_TIME} || time;
    return $time           if $string eq 'now';
    return $string         if $string =~ m/^\d+$/;     # epoch seconds
    return $time - 60 * $1 if $string =~ m/^(\d+)m/;
    return;
}

1;

=head1 EXPORTABLE SUBROUTINES

=head2 get_root

Returns the path to the root clk directory.
