#!/usr/bin/perl
use strict;
use warnings;
sub lock_file ($&);  # predeclare for some nicer syntax

my $root = shift;
die "clk-store-entry path_to_storage\n" if not defined $root;
die "$root is not a directory\n" if not -d $root;

# parse the input for an entry time (assume the input is well-formed)
my $entry = do { local $/; <STDIN> };
my $entry_time = extract_entry_time(\$entry)
    or die "No valid time in the entry\n";

# store the entry itself before linking it into the timeline
my $entry_id = calculate_entry_id(\$entry);
mkdir "$root/entries" if not -e "$root/entries";
my $hash = substr $entry_id, 0, 2;
my $rest = substr $entry_id, 2;
mkdir "$root/entries/$hash" if not -e "$root/entries/$hash";
open my $entry_fh, '>', "$root/entries/$hash/$rest"
    or die "Could not store entry in $root/entries/$hash/$rest: $!\n";
print {$entry_fh} $entry;
close $entry_fh;

# look through each timeline for one that can hold this entry
my $timelines_dir = "$root/timelines";
mkdir $timelines_dir if not -e $timelines_dir;
opendir my $dir, $timelines_dir
    or die "Cannot open $timelines_dir\n";
TIMELINE:
while ( my $timeline = readdir($dir) ) {
    next TIMELINE if substr($timeline,0,1) eq '.';
    next TIMELINE if $timeline gt $entry_time;

    # atomically append to this timeline (if possible)
    my $success = lock_file "$timelines_dir/$timeline", sub {
        open my $fh, '<', $_[0]
            or die "Cannot open $timelines_dir/$timeline for reading\n";
        my $content = do { local $/; <$fh> };
        if ($content) {
            my $time = substr $content, -50, 8;
            return if $entry_time le $time;  # go to the next timeline
        }

        # append this entry to the timeline
        open $fh, '>>', $_[0]
            or die "Cannot reopen timeline $_[0] for appending\n";
        print {$fh} "$entry_time $entry_id\n";
        print "$entry_id\n";
        return 1;  # success
    };
    next TIMELINE if not $success;
    exit;
}
closedir $dir;

# there are no candidate timelines, so create one (safely)
lock_file "$timelines_dir/$entry_time", sub {
    my ($fh, $filename) = tempfile();
    print {$fh} "$entry_time $entry_id\n";
    close $fh;
    rename $filename, $_[0];
    print "$entry_id\n";
    return;
};

################# helper subroutines ###################

sub extract_entry_time {
    my ($entry_ref) = @_;
    my @parts = $$entry_ref =~ m{
        ^
        time \s* : \s*
        (\d{4})-
        (\d{2})-
        (\d{2})T
        (\d{2}):
        (\d{2}):
        (\d{2})Z
        $
    }xms;
    require Time::Local;
    $parts[1]--;  # 0-based month
    my $time = Time::Local::timegm( reverse @parts );
    return sprintf '%08x', $time;
}

sub calculate_entry_id {
    my ($entry_ref) = @_;
    require Digest::SHA1;
    return Digest::SHA1::sha1_hex($$entry_ref);
}

sub lock_file ($&) {
    my ( $filename, $transaction ) = @_;

    # write our process ID to a temporary file
    my ( $fh, $name ) = tempfile();
    print {$fh} $$;
    close $fh;

    # atomically create the lock file
    my $lock_file = "$filename.lock";
    my $success = link( $name, $lock_file ) && ( stat $lock_file )[3] == 2;
    unlink $name;

    # die or call the transaction code (preserving call context)
    die "Could not lock $filename\n" if not $success;
    if (wantarray) {
        my @result = $transaction->($filename);
        unlink $lock_file;
        return @result;
    }
    my $result = $transaction->($filename);
    unlink $lock_file;
    return $result;
}

# creates a temporary file. caller is responsible for removing it
sub tempfile {
    my $name = "$$-" . int( rand 10_000 );
    require Fcntl;
    my $flags = Fcntl::O_CREAT() | Fcntl::O_EXCL() | Fcntl::O_RDWR();
    sysopen( my $fh, $name, $flags, 0600 )
      or die "Could not create a temporary file $name\n";
    return ($fh, $name);
}
