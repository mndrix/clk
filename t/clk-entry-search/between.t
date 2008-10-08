use strict;
use warnings;
use Test::More tests => 21;
use App::Clk::Test;

clk_setup_test();

# create some entries to use for testing
cmd_ok <<'...';   # 2008-10-07T03:36:06Z
$ ./clk in --at 1223350566
> 66c9b5ba2c335a34e42ce194944b529c391df9de
...
cmd_ok <<'...';   # 2008-10-07T03:36:30Z
$ ./clk in --at 1223350590
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...
cmd_ok <<'...';   # 2008-10-08T03:36:06Z
$ ./clk in --at 1223436966
> e51261ef2cacb06ff7704da7587e7ebf8d44b19b
...
# TODO create entries in a second and third timeline to test those cases

# search for entries between two epoch times
cmd_ok <<'...';
$ ./clk entry-search --between 1223350580 1223350595
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...
cmd_ok <<'...';
$ ./clk entry-search --between 1223350500 1223350595
> 66c9b5ba2c335a34e42ce194944b529c391df9de
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...

# try a couple error conditions
cmd_ok <<'...';
$ ./clk entry-search --between
! --between requires two arguments
? 255
...
cmd_ok <<'...';
$ ./clk entry-search --between 1223350580
! --between requires two arguments
? 255
...
