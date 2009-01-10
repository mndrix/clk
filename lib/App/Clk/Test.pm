package App::Clk::Test;
use strict;
use warnings;
use base qw( Exporter );

use Test::Builder;
use IPC::Open3 qw( open3 );
use File::Path qw( rmtree );
use File::chdir;
use POSIX qw( strftime );
use Time::Local qw( timelocal timegm );

BEGIN { our @EXPORT = qw(
    clk_setup_test
    cmd_ok
    fake_time
    files_ok
    iso
    touch_file
) };

# import is_deeply without everything in Test::More::import()
use Test::More ();
*is_deeply = \&Test::More::is_deeply;

# perform some setup which should be done before every clk test
sub clk_setup_test {
    my ($args) = @_;

    # create a fresh, safe root directory
    $ENV{CLK_ROOT} = 't/_clk';
    rmtree('t/_clk') if -d 't/_clk';
    mkdir 't/_clk';

    # create a testing identity
    $ENV{CLK_IDENTITY} = 'tester@example.org';

    # establish a fake time, if necessary
    if ( my $iso = $args->{fake_time} ) {
        fake_time($iso);
    }

    # create some sample data, if necessary
    if ( $args->{make_data} ) {
        generate_test_data();
    }

    # clean up after ourselves
    END {
        rmtree('t/_clk') if !$ENV{DEBUG} && -d 't/_clk';
    }

    return $ENV{CLK_ROOT};
}

# run a command and check the output, error and exit code
sub cmd_ok {
    my $spec = shift;
    my $args = shift || {};
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    my $Test = Test::More->builder;
    my @lines = split m{\n}, $spec;
    fake_time( $args->{at} ) if $args->{at};

    # parse the test specification
    my ( $cmd, @input, @output, @error );
    my $exit = 0;    # assume they want success, unless otherwise indicated
    for my $line (@lines) {
        $line =~ s/^\s+//;
        my ($type, $content) = $line =~ m/^([\$<>!?])\s*(.*)$/;
        if    ( $type eq '$' ) { $cmd       =  $content }
        elsif ( $type eq '<' ) { push @input,  $content }
        elsif ( $type eq '>' ) { push @output, $content }
        elsif ( $type eq '!' ) { push @error,  $content }
        elsif ( $type eq '?' ) { $exit      =  $content }
        else                   { die "Unknown spec line type: $type\n" }
    }

    # empty strings on the end represent trailing newline characters
    push @output, '' if @output;
    push @error, ''  if @error;

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
    is_deeply( [ split /\n/, $got_output, -1 ], \@output, "$cmd : stdout" )
        unless $args->{skip_tests};
    $got_error = '' if not defined $got_error;
    is_deeply( [ split /\n/, $got_error,  -1 ], \@error,  "$cmd : stderr" )
        unless $args->{skip_tests};
    $Test->cmp_ok( $got_exit, '==', $exit, "$cmd : exit code" )
        unless $args->{skip_tests};

    return;
}

# the same as cmd_ok, but don't run the tests
sub cmd {
    my $spec = shift;
    my $args = shift || {};
    $args->{skip_tests} = 1;
    cmd_ok($spec, $args);
}


# examine a filesystem for file and contents
sub files_ok {
    my ($description) = @_;
    my $Test = Test::More->builder;
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

# create a file relative to the clk root
sub touch_file {
    my ($filename) = @_;
    local $CWD = $ENV{CLK_ROOT};
    my @dirs = split m{/}, $filename;
    my $file = pop @dirs;
    for my $dir (@dirs) {
        mkdir $dir if not -e $dir;
        chdir $dir;
    }

    open my $fh, '>', $file or die "Cannot open file $file: $!\n";
    close $fh;
}

# make believe that it's the given time
sub fake_time {
    my ($iso) = @_;
    my @parts = split /[-T:Z]/, $iso;
    $parts[1]--;  # make the month zero-based
    $ENV{CLK_TIME} = $iso =~ /Z$/
                   ? timegm( reverse @parts )
                   : timelocal( reverse @parts )
                   ;

    return;
}

sub iso {
    my ($time) = @_;
    return q{} if not defined $time;
    return strftime( "%Y-%m-%dT%H:%M:%S", localtime($time) );
}

sub generate_test_data {

    # create some entries to use for testing
    cmd <<'...';   # 2008-10-07T03:36:06Z
    $ ./clk in --at 1223350566
    > 66c9b5ba2c335a34e42ce194944b529c391df9de
...
    cmd <<'...';   # 2008-10-07T03:36:30Z
    $ ./clk in --at 1223350590
    > 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...
    cmd <<'...';   # 2008-10-08T03:36:06Z
    $ ./clk in --at 1223436966
    > e51261ef2cacb06ff7704da7587e7ebf8d44b19b
...

    # create entries in a second timeline
    cmd <<'...';   # 2008-10-07T03:33:50Z
    $ ./clk in --at 1223350430
    > 235843b6de92528e47a21598be7f350aea5b1a04
...
    cmd <<'...';   # 2008-10-07T03:33:59Z
    $ ./clk in --at 1223350439
    > a38dc7e581bb22b3add7092952d6b4915ce8ba3c
...

    # create entries in a third timeline
    cmd <<'...';   # 2008-10-07T03:31:50Z
    $ ./clk in --at 1223350310
    > a9aef841af1b731b65bf172072952e1cad06503b
...
    cmd <<'...';   # 2008-10-07T03:32:00Z
    $ ./clk in --at 1223350320
    > 951cf39364677d67a28c519a08578184c75d5f52
...

}

1;
