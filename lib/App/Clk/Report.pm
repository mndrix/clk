package App::Clk::Report;
use strict;
use warnings;
use App::Clk::Util qw( to_localtime );

sub Report () { __PACKAGE__ }

sub import {
    my $pkg = caller;
    no strict 'refs';
    *{ $pkg . '::Report' } = \&Report;
    *{ $pkg . '::to_localtime' } = \&to_localtime;
    return;
}

sub new {
    my ($class) = shift;
    my @args = @_;
    @args = ('--period', 'today') if not @args;  # default to displaying today
    open my $search, '-|',
      'clk', 'entry-search', '--output', 'content,duration', @args
      or die "Couldn't run entry-search\n";

    return bless $search, $class;
}

sub next {
    my ($self) = @_;

    # extract the entry ID
    my $id_line = <$self>;
    return if not $id_line;
    chomp $id_line;
    my ($entry_id) = $id_line =~ m/^id ([0-9a-zA-Z]+)$/;

    # is there a duration line
    chomp( my $duration_line = <$self> );
    my $duration;
    my $length_line;
    if ( $duration_line =~ m/^duration (\d+)$/ ) {
        $duration = $1;
        chomp( $length_line = <$self> );
    }
    else {
        $length_line = $duration_line;
    }

    # how much content is there in this entry?
    my ($content_length) = $length_line =~ m/^content (\d+)$/;

    # slurp the entry's content
    my $entry_text = do {
        local $/ = \$content_length;  # slurp the entry's content
        <$self>;
    };

    # convert the entry text into a hash
    my $entry = parse_entry($entry_text);
    $entry->{id} = $entry_id;
    $entry->{duration} = $duration if defined $duration;

    return $entry;
}

sub parse_entry {
    my ($entry_text) = @_;

    my %entry;
    for my $line ( split m{$/}, $entry_text ) {
        my ($key, $value) = split /: /, $line;
        $entry{$key} = $value;
    }

    return \%entry;
}

1;

__END__

=head1 NAME

App::Clk::Report - helper routines for reports

=head2 SYNOPSIS

    use App::Clk::Report;
    my $rpt = Report->new(@ARGV);
    while ( my $entry = $rpt->next ) {
        print "date = $entry->{date}\n";
    }
