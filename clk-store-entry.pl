#!/usr/bin/perl
use strict;
use warnings;

my $root = shift;
die "clk-store-entry path_to_storage\n" if not defined $root;
die "$root is not a directory\n" if not -d $root;

# parse the input for an entry time (assume the input is well-formed)
my $entry = do { local $/; <STDIN> };
my $entry_time = extract_entry_time(\$entry)
    or die "No valid time in the entry\n";

# look through each timeline for one that can hold this entry
my $timelines_dir = "$root/timelines";
opendir my $dir, $timelines_dir
    or die "Cannot open $timelines_dir\n";
TIMELINE:
while ( my $timeline = readdir($dir) ) {
    next TIMELINE if substr($timeline,0,1) eq '.';
    next TIMELINE if $timeline gt $entry_time;
    open my $fh, '<', "$timelines_dir/$timeline"
        or die "Cannot open $timelines_dir/$timeline for reading\n";
    my $content = do { local $/; <$fh> };
    if ($content) {
        my $time = substr $content, -50, 8;
        next TIMELINE if $entry_time le $time;
    }

    # reopen for appending and append
    # TODO lock the timeline
    open $fh, '>>', "$timelines_dir/$timeline"
        or die "Cannot reopen $timelines_dir/$timeline for appending\n";
    my $id = calculate_entry_id(\$entry);
    print {$fh} "$entry_time $id\n";
    print "$id\n";
    exit;
}
closedir $dir;

# there are no candidate timelines, so create one
require File::Temp;
my ($fh, $filename) = File::Temp::tempfile();
my $id = calculate_entry_id(\$entry);
print {$fh} "$entry_time $id\n";
print "$id\n";
close $fh;
rename $filename, "$timelines_dir/$entry_time";

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
