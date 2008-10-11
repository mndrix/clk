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

sub resolve_period {
    my ($period) = shift;
    $period = q{} if not defined $period;
    my $time = $ENV{CLK_TIME} || time;

    # a list of coderefs for handling date periods
    my @handlers = (
        \&resolve_period_today,
    );

    # ask each handler if it understands this pattern
    for my $handler (@handlers) {
        my ($begin, $end) = $handler->( $time, $period );
        return ($begin, $end) if defined $begin;
    }

    # none of the handlers understood this period
    die "Unknown period description: $period\n";
}

sub resolve_period_today {
    my ($time, $period) = @_;
    return if $period !~ m/^today$/i;

    my ( @begin_parts, @end_parts );
    @begin_parts = @end_parts = localtime($time);
    @begin_parts[ 0, 1, 2 ] = (  0,  0,  0 );    # beginning of the day
    @end_parts[   0, 1, 2 ] = ( 59, 59, 23 );    # end of the day

    require Time::Local;
    return (
        Time::Local::timelocal(@begin_parts),
        Time::Local::timelocal(@end_parts)
    );
}

1;

=head1 EXPORTABLE SUBROUTINES

=head2 get_root

Returns the path to the root clk directory.
