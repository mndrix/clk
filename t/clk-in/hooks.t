use strict;
use warnings;
use Test::More tests => 16;
use App::Clk::Test;

my $root = clk_setup_test({
    fake_time => '2008-03-19T12:34:56Z',
});

# without a hooks directory
my $no_hooks = <<'...';
$ ./clk in --output-only whatever
> time: 2008-03-19T12:34:56Z
> text: whatever
...
cmd_ok $no_hooks;

# an empty hooks directory
mkdir "$root/hooks";
cmd_ok $no_hooks;

# a simple hook
make_hook( 'pre-in' => <<"..."
#!$^X 
print "arguments: \@ARGV\n";
...
);
cmd_ok <<'...';
$ ./clk in --output-only whatever
> time: 2008-03-19T12:34:56Z
> arguments: whatever
...

# a hook that prevents the entry from being stored
my $hook = make_hook( 'pre-in' => <<"..."
#!$^X 
warn "This is an error message\n";
exit 7;
...
);
cmd_ok <<'...';
$ ./clk in uh-oh
! The hook 'pre-in' stopped the entry with the message:
! This is an error message
? 7
...
my @entries = glob("$root/entries/*");
cmp_ok scalar @entries, '==', 0, 'no entries';

# a hook that's not executable
chmod 0666, $hook;
cmd_ok $no_hooks;


#########################################################################

use App::Clk::Util qw( clk_root );
sub make_hook {
    my ( $name, $content ) = @_;
    my $root = clk_root();
    my $path = "$root/hooks/$name";
    open my $fh, '>', $path;
    print $fh $content;
    close $fh;
    chmod 0777, $path;
    return $path;
}
