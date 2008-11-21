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

# returns the path of the root of the clk directory
sub clk_root {
    my $root = $ENV{CLK_ROOT}
        or die "Please set CLK_ROOT before using clk\n";
    die "CLK_ROOT=$root is not a directory\n" if not -d $root;
    return $root;
}

# given a single user identity, returns the path of the root of the timeline
# directory
sub timeline_root {
    my ($user_identity) = @_;
    my $root = clk_root();
    require Digest::SHA1;
    my $user_hash = Digest::SHA1::sha1_hex($user_identity);

    return hashed_path( $user_hash, '%r/timelines/%h' );
}

# given a string representation of an instant in time, it returns
# that instant as the number of seconds since the epoch.  Supported
# instant formats are:
#
#   * a number of seconds since the epoch
#   * a number followed by the letter "m" indicates a number of
#     minutes in the past
sub resolve_instant {
    my ($string) = @_;
    my $time = $ENV{CLK_TIME} || time;
    return $time           if $string eq 'now';
    return $string         if $string =~ m/^\d+$/;     # epoch seconds
    return $time - 60 * $1 if $string =~ m/^(\d+)m/;

    die "Unknown instant description: $string\n";
}

=head2 resolve_period

Given an English-language description of a period of time, it returns two
times in epoch seconds.  The first time is the starting time of the period.
The second time is the ending time of the period.

If the period description is not recognized, an exception is thrown.

=cut

sub resolve_period {
    my ($period) = shift;
    $period = q{} if not defined $period;
    my $time = $ENV{CLK_TIME} || time;

    # a list of coderefs for handling date periods
    my @handlers = (
        \&resolve_period_day,
        \&resolve_period_week,
        \&resolve_period_month,
        \&resolve_period_year,
        \&resolve_period_ever,
    );

    # ask each handler if it understands this pattern
    for my $handler (@handlers) {
        my ($begin, $end) = $handler->( $time, $period );
        return ($begin, $end) if defined $begin;
    }

    # none of the handlers understood this period
    die "Unknown period description: $period\n";
}

sub resolve_period_day {
    my ($time, $period) = @_;
    $period = lc $period;

    if ( $period =~ m/^(yester|to)day$/ ) {
        $time -= 24*60*60 if $1 eq 'yester';
    }
    elsif ( $period =~ m/^(sun|mon|tues|wednes|thurs|fri|satur)day$/ ) {
        # Understand the name of a day of the week to mean the first such day
        # in the past.  If the name of today is given, assume that the user
        # meant the previous one; otherwise, she would have said "today"
        my $today = ( localtime $time )[6];    # today's week day number
        my @prefixes = qw( sun mon tues wednes thurs fri satur );
        my $goal = 0;                          # goal's week day number
        for my $prefix (@prefixes) {
            last if $1 eq $prefix;
            $goal++;
        }
        my $days = $today - $goal;
        $days += 7 if $days <= 0;
        $time -= $days * 24*60*60;
    }
    else {  # we don't understand this period format
        return;
    }

    return enclosing_day($time);
}

sub resolve_period_week {
    my ($time, $period) = @_;
    my ($which) = $period =~ m/^(this|last) week$/;
    return if not $which;
    if ( $which eq 'last' ) {
        $time -= 7*24*60*60;  # go back into the previous week
    }
    return enclosing_week($time);
}

sub resolve_period_month {
    my ($time, $period) = @_;
    my ($which) = $period =~ m/^(this|last) month$/;
    return if not $which;
    if ( $which eq 'last' ) {
        my $day = ( localtime $time )[3];
        $time -= ( $day + 1 )*24*60*60;  # go back into the previous month
    }
    return enclosing_month($time);
}

sub resolve_period_year {
    my ($time, $period) = @_;
    my ($which) = $period =~ m/^(this|last) year$/;
    return if not $which;
    if ( $which eq 'last' ) {
        my @parts = localtime $time;
        $parts[5]--;    # go back one year
        $parts[3] = 1;  # first day (to avoid leap day problems)
        $parts[0] = 0;  # first second (to avoid leap second problems)
        require Time::Local;
        $time = Time::Local::timelocal(@parts);
    }
    return enclosing_year($time);
}

sub resolve_period_ever {
    my ($time, $period) = @_;
    $period = lc $period;
    return if $period ne 'ever';

    return ( 0, 2**31-1 );
}

# a helper subroutine for 'today', 'yesterday', etc.  It finds the
# day which includes the given time and returns the starting and ending
# second of that day.
sub enclosing_day {
    my ($time) = @_;

    # choose the beginning and ending seconds of the day
    my ( @begin_parts, @end_parts );
    @begin_parts = @end_parts = localtime($time);
    @begin_parts[ 0, 1, 2 ] = (  0,  0,  0 );    # beginning of the day
    @end_parts[   0, 1, 2 ] = ( 59, 59, 23 );    # end of the day

    # convert the date parts back into epoch seconds
    require Time::Local;
    return (
        Time::Local::timelocal(@begin_parts),
        Time::Local::timelocal(@end_parts)
    );
}

# similar to enclosing_day, but it finds the enclosing week
sub enclosing_week {
    my ($time) = @_;

    # find the preceding Monday at midnight
    my $wday = ( localtime $time )[6];
    my $monday = $time - 24*60*60 * ( ( $wday + 6 ) % 7 );    # make Monday=0
	my @begin_parts = localtime($monday);
    @begin_parts[ 0, 1, 2 ] = ( 0, 0, 0 );

    # find the end of the following Sunday
	my $sunday = $monday + 24*60*60 * 6;
	my @end_parts = localtime($sunday);
    @end_parts[ 0, 1, 2 ] = ( 59, 59, 23 );

    # convert the date parts back into epoch seconds
    require Time::Local;
    return (
        Time::Local::timelocal(@begin_parts),
        Time::Local::timelocal(@end_parts)
    );
}

# similar to enclosing_day, but it finds the enclosing month
sub enclosing_month {
    my ($time) = @_;
    require Time::Local;

    # find the first day of this month
    my $first_day_of = sub {
        my @parts = localtime shift();
        @parts[0, 1, 2, 3] = ( 0, 0, 0, 1 );
        return Time::Local::timelocal(@parts);
    };
    my $first_day = $first_day_of->($time);

    # next month - 1 second = last day of this month
    my $last_day = $first_day_of->( $first_day + 32*24*60*60 ) - 1;

    # find the end of that last day
	my @end_parts = localtime($last_day);
    @end_parts[ 0, 1, 2 ] = ( 59, 59, 23 );

    # convert the date parts back into epoch seconds
    return (
        $first_day,
        Time::Local::timelocal(@end_parts)
    );
}

# similar to enclosing_day, but it finds the enclosing year
sub enclosing_year {
    my ($time) = @_;

    # find the first day of the year
    my @begin_parts = localtime($time);
    @begin_parts[ 0, 1, 2, 3, 4 ] = ( 0, 0, 0, 1, 0 );

    # find the last day of the year
    my @end_parts = @begin_parts;
    @end_parts[ 0, 1, 2, 3, 4 ] = ( 59, 59, 23, 31, 11 );

    # convert the date parts back into epoch seconds
    require Time::Local;
    return (
        Time::Local::timelocal(@begin_parts),
        Time::Local::timelocal(@end_parts)
    );
}

=head2 hashed_path( $sha1, $template )

    my $path = hashed_path( $sha1, '%r/entries/%h' );

Returns an absolute path to the filesystem where part of the path is a hashed
directory.  The hashed directory makes it possible to store many files within
multiple subdirectories.  This makes certain filesystem opertions more
efficient.

C<$sha1> should be the SHA-1 hash of some value.  C<$template> is a template
discribing how the hashed directory structure should be converted to a path.
The template uses escapes similar to L<printf>.  Acceptable escapes are:

=head3 %h

The hashed directory component.  If C<$sha1> were
f1d2d2f924e986ac86fdf7b36c94bcdf32beec15, this escape would expand to
"f1/d2d2f924e986ac86fdf7b36c94bcdf32beec15"

=head3 %r

The root directory for storing clk content.  This is typically the first
component in C<$template>.

=cut

sub hashed_path {
    my ($sha1, $template) = @_;

    my $hash = substr $sha1, 0, 2;
    my $rest = substr $sha1, 2;

    $template =~ s{%r}{clk_root()}eg;  # %r
    $template =~ s{%h}{$hash/$rest}g;  # %h

    return $template;
}

1;

=head1 EXPORTABLE SUBROUTINES

=head2 get_root

Returns the path to the root clk directory.
