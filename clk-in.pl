#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
use POSIX qw( strftime );
use Time::HiRes qw( gettimeofday );

my $message = '';
my @tags;
GetOptions(
    'm=s' => \$message,
    't=s' => \@tags,
);

die "Your message may not contain tabs\n" if $message =~ m/\t/;
for my $tag (@tags) {
    die "Your tags may not contain commas\n" if $tag =~ m/,/;
    die "Your tags may not contain tabs\n"   if $tag =~ m/\t/;
}

my @parts;
push @parts, sprintf( "identity:%s", 'michael@ndrix.com' );
push @parts, sprintf( "time:%s",     iso8601_now() );
push @parts, sprintf( "tags:%s",     join( ',', @tags ) ) if @tags;
push @parts, sprintf( "message:%s",  $message );
my $line = join "\t", @parts;
print "$line\n";

sub iso8601_now {
    my ( $seconds, $microseconds ) = gettimeofday();
    my $fraction = sprintf( '%06d', $microseconds );
    return strftime( "%FT%T.${fraction}Z", gmtime($seconds) );
}
