package App::Clk::Util;
use strict;
use warnings;

=head1 NAME

App::Clk::Util - utility functions for clk

=head1 FUNCTIONS

=cut

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

# returns the current time (accounting for environment overrides)
sub now {
    return $ENV{CLK_TIME} || time;
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

=head2 to_localtime($iso)

Parses an ISO datetime string and returns the equivalent time in epoch
seconds.

=cut

sub to_localtime {
    my ($iso) = @_;
    my @parts = reverse split /[TZ:-]/, $iso;
    $parts[4]--;  # month is 0-based
    $parts[5] -= 1900;  # year is 1900-based

    require Time::Local;
    return scalar Time::Local::timegm(@parts);
}

# given a string representation of an instant in time, it returns
# that instant as the number of seconds since the epoch.  Supported
# instant formats documented in docs/specs.pod
sub resolve_instant {
    my ($string) = @_;
    my $time = now();
    return $time           if $string eq 'now';
    return $string         if $string =~ m/^\d+$/;     # epoch seconds
    return $time - 60 * $1 if $string =~ m/^(\d+)m/;

    # can we resolve the instant as an entry?
    my $entries = entry_search([$string]);
    my $entry = $entries->next;
    die "Unknown instant description: $string\n" if not $entry;
    die   "Resolving instant as entry pattern '$string': "
        . "more than one matching entry\n"
        if $entries->next;

    return $entry->time;
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
    my $time = now();

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

=head2 entry_search($positional, $named)

Given an arrayref of C<$positional> arguments and a hashref of C<$named>
arguments (both optional), returns an iterator (see L</Iterator Interface>)
which produces entry objects (see L</Entry Interface>).

=cut

sub entry_search {
    my ($positional) = grep { ref $_ eq 'ARRAY' } @_;
    my ($named)      = grep { ref $_ eq 'HASH'  } @_;

    # build a command-line for clk-entry-search
    my @arguments;
    $named ||= {};
    while ( my ($key, $value) = each %$named ) {
        push @arguments, "--$key", ( ref $value ? @$value : $value );
    }
    push @arguments, @{ $positional || [] };
    unshift @arguments, '--output content,duration';  # optimize later

    # let clk-entry-search do the hard work
    my $command = join ' ', './clk', 'entry-search', @arguments;
    open my $fh, '-|', $command
        or die "Unable to run clk entry-search";
    return iterator( sub {
        my $id = <$fh>;
        return if not defined $id;
        ($id) = $id =~ m/^id ([a-f0-9]+)$/m;
        my ($duration) = <$fh> =~ m/^duration (\d+)$/m;
        my ($length)   = <$fh> =~ m/^content (\d+)$/m;
        local $/ = \$length;
        my $content = <$fh>;
        my ($time) = grep { /^time: / } split( /\n/, $content );
        $time =~ s/^time: //;
        return App::Clk::Entry->new({
            duration => $duration,
            time     => to_localtime($time),
            id       => $id,
        });
    });
}

=head2 append_deletions_log($kind, $entry_id)

Appends a single entry to the deletions log.  C<$kind> is 'd' to indicate
deletion and 'u' to indicate undeletion.  C<$entry_id> specifies which entry
has been un/deleted.

=cut

sub append_deletions_log {
    my ($kind, $entry_id) = @_;
    die "'$kind' is not a valid deletion log entry kind\n"
      if $kind ne 'd' and $kind ne 'u';

    my $root = clk_root();
    my $log = "$root/deletions.log";
    open my $fh, '>>', $log
      or die "Unable to append the deletions log $log\n";
    print $fh "$kind $entry_id\n";

    return;
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

=head2 parse_timeline_line($line)

Given a single line from a timeline file, returns a list.  The first
member is the epoch time for that line.  The second member is the entry ID.
If C<$line> cannot be parsed, an exception is thrown.

=cut

sub parse_timeline_line {
    my ($line) = @_;
    if ( $line =~ m/^([0-9a-f]{8}) ([0-9a-f]{40})$/o ) {
        return ( hex($1), $2 );
    }
    die "Timeline entry is invalid: $line\n";
}

=head2 is_deleted_entry($entry_id)

Returns a true value if C<$entry_id> is marked as being deleted.  Otherwise,
it returns a false value.

=cut

sub is_deleted_entry {
    my ($entry_id) = @_;
    our $deleted_entries;
    if ( not $deleted_entries ) {
        my $log = clk_root() . '/deletions.log';
        if ( -e $log ) {
            open my $fh, '<', $log
              or die "Unable to open deletions log $log: $!\n";
            while ( my $line = <$fh> ) {
                chomp $line;
                my ($kind, $entry_id) = split / /, $line;
                $deleted_entries->{$entry_id} = $kind eq 'd' ? 1 : 0;
            }
        }
        else {
            $deleted_entries = {};
            return;
        }
    }

    return $deleted_entries->{$entry_id};
}

=head1 Iterator Subroutines

=head2 iterator($code)

Given a code reference, returns an iterator object whose values are obtained
by calling C<$code> repeatedly.

=cut

sub iterator {
    my ($code) = @_;
    return App::Clk::Util::Iterator->new($code);
}

=head2 iterator_file_lines( $path, $direction )

Given a filesystem C<$path> and a C<$direction> (either 'forwards' or
'backwards'), returns an iterator which produces a single, chomped line
from the file at a time.

=cut

sub iterator_file_lines {
    my $path = shift;
    die "You must specify a path" if not defined $path;
    my $direction = shift || '';
    $direction = $direction eq 'forwards'  ? +1
               : $direction eq 'backwards' ? -1
               : die "Invalid direction '$direction'"
               ;

    open my $fh, '<', $path or die "Could not open $path: $!";
    my @lines = $direction > 0 ? <$fh> : reverse <$fh>;
    my $i = 0;  # start at the first line
    return iterator( sub {
        return if $i > $#lines;
        chomp( my $line = $lines[$i++] );
        return $line;
    });
}

=head2 iterator_merge($choose, @iterators)

Returns an iterator representing the merge of all C<@iterators>.

C<@iterators> is a list of iterators.  C<$choose> is a code reference for
choosing one value among several possibilities.  Each time that a value is
needed from the merged iterator, C<$choose> is called with an arrayref of
possible values.  It should return the index of the chosen value.

=cut

sub iterator_merge {
    my ($choose, @iterators) = @_;

    return iterator( sub {
        my @values;
        my $i = 0;
        while ( $i <= $#iterators ) {
            my $value = $iterators[$i]->peek;
            if ( defined $value ) {
                push @values, $value;
                $i++;
            }
            else {  # this iterator is empty, remove it
                splice @iterators, $i, 1;
            }
        }
        return if not @values;

        my $chosen = $choose->(\@values);
        $iterators[$chosen]->next;
        return $values[$chosen];
    });
}

=head2 iterator_sorted_merge($comparator, @iterators)

Similar to L</iterator_merge>, but the choice closure simply compares two
values.  Among several choices, the lesser value is always chosen.

If all C<@iterators> produce values in order sorted according to
C<$comparator>, L</iterator_sorted_merge> results in an iterator whose values
are also sorted that same way.

=cut

sub iterator_sorted_merge {
    my ($comparator, @iterators) = @_;

    return iterator_merge(
        sub {
            my $choices = shift;
            my $min_i   = 0;
            my $min     = $choices->[0];
            return if not defined $min;
            for my $i ( 1 .. $#$choices ) {
                if ( $comparator->( $min, $choices->[$i] ) > 0 ) {
                    $min_i = $i;
                    $min   = $choices->[$i];
                }
            }

            return $min_i;
        },
        @iterators
    );
}

=head2 iterator_timeline($identity, $direction)

Returns an iterator which iterates the entries in the timeline for the
identity named C<$identity>.  C<$direction> either 'forwards' or 'backwards'
(defaulting to 'forwards' if not given).  It specifies whether timeline
entries are returned in chronologically increasing or decreasing order,
respectively.

=cut

sub iterator_timeline {
    my ($identity, $direction) = @_;
    $direction ||= 'forwards';
    my $timeline_root = timeline_root($identity);
    my @iterators;
    for my $path ( glob "$timeline_root/*" ) {
        my $lines = iterator_file_lines( $path, $direction );
        push @iterators, iterator( sub {
            my $line = $lines->next;
            return if not defined $line;
            return [ parse_timeline_line($line) ];
        });
    }

    my $comparator
        = $direction eq 'forwards'  ? sub { $_[0][0] <=> $_[1][0] }
        : $direction eq 'backwards' ? sub { $_[1][0] <=> $_[0][0] }
        : die "Invalid timeline direction\n"
        ;
    return iterator_sorted_merge( $comparator, @iterators );
}

package App::Clk::Util::Iterator;

=head1 Iterator Interface

=head2 new

Don't use this method.  See L</iterator>, L</iterator_merge> and
L</iterator_sorted_merge> for details about creating iterator objects.

=cut

sub new {
    my ($class, $code) = @_;
    return bless [ undef, $code ], $class;
}

=head2 peek

Returns the value that L</next> will return the next time it's called.

=cut

sub peek {
    my ($self) = @_;
    return $self->[0] if defined $self->[0];
    return $self->[0] = $self->[1]->();
}

=head2 next

Returns the next value from this iterator.

=cut

sub next {
    my ($self) = @_;
    my $value = $self->peek;
    $self->[0] = undef;
    return $value;
}

=head2 take($n)

Returns a list of C<$n> values from the iterator.  It's assumed that the
iterator can produce at least C<$n> values.  This can be handy with infinite
iterators.

=cut

sub take {
    my ($self, $count) = @_;
    my @values;
    push @values, $self->next for 1 .. $count;
    return @values;
}

=head2 all

Returns a list of all the values from the iterator.  Naturally, this method
should not be called on infinite iterators.

=cut

sub all {
    my ($self) = @_;
    my @values;
    push @values, $self->next while defined $self->peek;
    return @values;
}

package App::Clk::Entry;

=head1 Entry Interface

=head2 new

Don't use this method.  See L</entry_search> to obtain entry objects.

=cut

sub new {
    my ($class, $args) = @_;
    return bless $args, $class;
}

sub id       { shift->{id} }
sub time     { shift->{time} }
sub duration { shift->{duration} }

1;
