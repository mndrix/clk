use strict;
use warnings;
use Test::More tests => 12;
use App::Clk::Test;

# a basic test of STDOUT
cmd_ok <<'...';
$ perl -e 'print "foo\n"'
> foo
? 0
...

# a basic test of STDERR
cmd_ok <<'...';
$ perl -e 'print STDERR "on error\n"'
! on error
? 0
...

# a basic test of the exit code
cmd_ok <<'...';
$ perl -e 'exit 1'
? 1
...

# echo STDIN to STDOUT (test for default exit code)
cmd_ok <<'...';
$ perl -e 'print join("", <STDIN>)'
< footasticness
< is pretty cool
> footasticness
> is pretty cool
...
