use strict;
use warnings;
use Test::More tests => 6;

# some initial set up for this test
use File::Path qw( rmtree );
mkdir 't/swamp';
chdir 't/swamp';
END { chdir '../..'; rmtree('t/swamp') };
$ENV{CLK_ROOT} = 'root';
$ENV{PATH}     = ".:$ENV{PATH}";
sub run {
    return App::Clk::CmdReturn->new(@_);
}
package App::Clk::CmdReturn;
use base 'Class::Accessor';
use IPC::Cmd;  # TODO just use IPC::Open3 directly (lots of dependencies here)
__PACKAGE__->mk_accessors(qw( success error_code full_output out err ));
sub new {
    my ($class, $command) = @_;
    my ( $success, $error, $full, $out, $err ) =
      IPC::Cmd::run( command => $command );
    my $self = bless {}, $class;
    $self->success($success);
    $self->error_code($error);
    $self->full_output($full);
    $self->out($out);
    $self->err($err);
    return $self;
}
package main;  # back to your regularly scheduled package

# some tests of the testing framework
{
    my $result = run("echo foo");
    ok $result->success, '"echo foo" exited';
    is_deeply $result->out, ["foo\n"], '... with stdout';
    is_deeply $result->err, [], '... no stderr';
}
{
    my $result = run("echo -n bar");
    ok $result->success, '"echo -n bar" worked';
    is_deeply $result->out, ['bar'], '... with stdout';
    is_deeply $result->err, [], '... no stderr';
}


# TODO write tests for clk-store-entry
# TODO I'd like to be able to do something like the following
#
#     my $result = run( 'clk store-entry', "time: 2008-03-21T12:34:56Z\n" );
#     cmp_methods $result, [
#         error_code => undef,
#         success    => bool(1),
#         out        => [ "44a28b594b4853b6cc0c671c3fa31b12d9abcdb4\n" ],
#         err        => [],
#     ], 'some message';
#
# and have the 'time:' string provided to the program as STDIN

# TODO or maybe something that looks more like a command line:

cmd_test <<'...'
$ clk store-entry
< time: 2008-03-21T12:34:56Z
> 44a28b594b4853b6cc0c671c3fa31b12d9abcdb4
!
? 0
...
