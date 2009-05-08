#!/usr/bin/perl
use strict;
use warnings;
use lib 'lib';
use App::Clk;
use App::Clk::Event;
use Getopt::Long;

my $subject = '';
my @tags;
GetOptions(
    'm=s' => \$subject,
    't=s' => \@tags,
);

my $storage = App::Clk->storage;
my $event   = App::Clk::Event->new({
    owner   => 'michael@ndrix.org',
    subject => $subject,
    tags    => \@tags,
});
$storage->add($event);
