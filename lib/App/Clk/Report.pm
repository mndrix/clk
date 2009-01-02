package App::Clk::Report;
use strict;
use warnings;

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
    open my $search, '-|', qw( clk entry-search --output content ), @args
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

    # how much content is there in this entry?
    chomp( my $length_line = <$self> );
    my ($content_length) = $length_line =~ m/^content (\d+)$/;

    # slurp the entry's content
    my $entry_text = do {
        local $/ = \$content_length;  # slurp the entry's content
        <$self>;
    };

    # convert the entry text into a hash
    my $entry = parse_entry($entry_text);
    $entry->{id} = $entry_id;

    return $entry;
}

sub to_localtime {
    my ($iso) = @_;
    my @parts = reverse split /[TZ:-]/, $iso;
    $parts[4]--;  # month is 0-based
    $parts[5] -= 1900;  # year is 1900-based

    require Time::Local;
    return scalar Time::Local::timegm(@parts);
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
