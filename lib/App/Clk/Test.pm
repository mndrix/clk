package App::Clk::Test;
use strict;
use warnings;
use base qw( Exporter );

use Test::Builder;
use IPC::Open3 qw( open3 );

BEGIN { our @EXPORT = qw( cmd_ok ) };
my $Test = Test::Builder->new;

# import is_deeply without everything in Test::More::import()
use Test::More ();
*is_deeply = \&Test::More::is_deeply;

sub cmd_ok {
    my ($spec) = @_;
    my @lines = split m{\n}, $spec;

    # parse the test specification
    my ( $cmd, @input, @output, @error );
    my $exit = 0;    # assume they want success, unless otherwise indicated
    for my $line (@lines) {
        my ($type, $content) = $line =~ m/^([\$<>!?])\s*(.*)$/;
        if    ( $type eq '$' ) { $cmd       =  $content }
        elsif ( $type eq '<' ) { push @input,  $content }
        elsif ( $type eq '>' ) { push @output, $content }
        elsif ( $type eq '!' ) { push @error,  $content }
        elsif ( $type eq '?' ) { $exit      =  $content }
        else                   { die "Unknown spec line type: $type\n" }
    }

    # run the command and capture input, output, error and exit code
    my ($write_fh, $read_fh, $err_fh);
    $err_fh = 1;  # so that STDERR is not redirected to STDOUT
    my $pid = open3 $write_fh, $read_fh, $err_fh, $cmd;
    my $got_output = '';
    my $got_error  = '';
    my $got_exit;
    print {$write_fh} join("\n", @input);
    close $write_fh;
    $got_output = do { local $/; <$read_fh> };
    $got_error  = do { local $/; <$err_fh>  };
    waitpid $pid, 0;
    $got_exit = $? >> 8;

    $got_output = '' if not defined $got_output;
    is_deeply( [ split /\n/, $got_output ], \@output, "$cmd : stdout" );
    $got_error = '' if not defined $got_error;
    is_deeply( [ split /\n/, $got_error  ], \@error,  "$cmd : stderr" );
    $Test->cmp_ok( $got_exit, '==', $exit, "$cmd : exit code" );

    return;
}

1;
