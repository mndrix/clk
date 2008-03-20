#!/usr/bin/perl
use strict;
use warnings;
use IPC::Open2;

# open clk-store-entry
my $pid = open2(my $reader, my $writer, './clk-store-entry.pl', './play');

# generate the entry
my @parts = (gmtime)[ 0 .. 5];
$parts[4]++;  # use human-readable day
$parts[5] += 1900; # ... and year
printf {$writer} "time: %04d-%02d-%02dT%02d:%02d:%02dZ\n", reverse @parts;
close $writer;  # send EOF

my $entry_id = <$reader>;
waitpid $pid, 0;
print $entry_id if $entry_id;
