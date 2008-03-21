#!/usr/bin/perl
use strict;
use warnings;

$ENV{CLK_ROOT} = './play';

# if testing, put the current directory in the path
$ENV{PATH} = ".:$ENV{PATH}" if substr($0, 0, 1) eq '.';

my $command = shift or die "Please specfy a clk command\n";

exec "clk-$command.pl", @ARGV;