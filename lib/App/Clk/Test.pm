package App::Clk::Test;
use strict;
use warnings;
use base qw( Exporter );

use Test::Builder;
use IPC::Open3 qw( open3 );
use File::Path qw( rmtree );
use File::chdir;
use Time::Local qw( timegm );

BEGIN { our @EXPORT = qw( clk_setup_test cmd_ok files_ok ) };
my $Test = Test::Builder->new;

# import is_deeply without everything in Test::More::import()
use Test::More ();
*is_deeply = \&Test::More::is_deeply;

# perform some setup which should be done before every clk test
sub clk_setup_test {
    my ($args) = @_;

    # create a fresh, safe root directory
    $ENV{CLK_ROOT} = 't/_clk';
    mkdir 't/_clk' if not -d 't/_clk';

    # establish a fake time, if necessary
    if ( my $iso = $args->{fake_time} ) {
        my @parts = split /[-T:Z]/, $iso;
        $parts[1]--;
        $ENV{CLK_TIME} = timegm( reverse @parts );
    }

    # clean up after ourselves
    END {
        rmtree('t/_clk') if !$ENV{DEBUG} && -d 't/_clk';
    }
}

# run a command and check the output, error and exit code
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
    local $SIG{PIPE} = sub {
        die "Broken pipe. Perhaps the following command wasn't "
          . "expecting input on STDIN: $cmd\n";
    };
    my $got_output = '';
    my $got_error  = '';
    my $got_exit;
    print {$write_fh} join( "\n", @input ) . "\n"
        if @input;
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

# examine a filesystem for file and contents
sub files_ok {
    my ($description) = @_;
    local $CWD = $ENV{CLK_ROOT};

    my @lines = split /\n/, $description;
    LINE:
    while ( my $line = shift @lines ) {
        next LINE if $line =~ m/^\s+/;  # we're not ready for file contents
        chomp( my $filename = $line );
        if ( not -e $filename ) {
            $Test->ok( 0, "$filename exists" );
            next LINE;
        }

        # what should the file contents be?
        my @expected;
        my $contents = shift @lines;
        my ($indent) = $contents =~ m/^(\s+)/;
        $contents =~ s/^$indent//;
        push @expected, $contents;
        CONTENT_LINE:
        while ( my $content_line = shift @lines ) {
            if ( $content_line =~ s/^$indent// ) {
                push @expected, $content_line;
            }
            else {
                unshift @lines, $content_line;
                last CONTENT_LINE;
            }
        }

        # compare the file contents to the expected contents
        open my $fh, '<', $filename or die "Cannot open $filename : $!\n";
        my @got = map { chomp ( my $v = $_ ); $v } <$fh>;
        is_deeply( \@got, \@expected, "$filename contents" );
    }
}

1;
