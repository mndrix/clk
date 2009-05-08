package App::Clk::Event;
use strict;
use warnings;
use POSIX qw( strftime );
use Time::HiRes qw( gettimeofday );

sub new {
    my ( $class, $args ) = @_;
    my $owner = $args->{owner} or die "No owner\n";
    my $subject = $args->{subject};
    my $tags    = $args->{tags} || [];
    die "Neither tags nor subject\n" unless $subject or @$tags;

    return bless {
        owner   => $owner,
        time    => iso8601_now(),
        subject => $subject,
        tags    => [ sort @$tags ],
    }, $class;
}

sub get_owner   { shift->{owner}    }
sub get_time    { shift->{time}     }
sub get_subject { shift->{subject}  }

sub get_tags {
    my ($self) = @_;
    return wantarray ? @{$self->{tags}} : $self->{tags};
}

sub get_tags_string {
    my ($self) = @_;
    return join ', ', $self->get_tags;
}

# helper sub
sub iso8601_now {
    my ( $seconds, $microseconds ) = gettimeofday();
    my $fraction = sprintf( '%06d', $microseconds );
    return strftime( "%FT%T.${fraction}Z", gmtime($seconds) );
}

1;
