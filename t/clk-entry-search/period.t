use strict;
use warnings;
use Test::More tests => 12;
use App::Clk::Test;

clk_setup_test({ make_data => 1 });

# search for some entries with a non-epoch instant
cmd_ok <<'...', { at => '2008-10-07T03:37:00Z' };
$ ./clk entry-search --period today
> a9aef841af1b731b65bf172072952e1cad06503b
> 951cf39364677d67a28c519a08578184c75d5f52
> 235843b6de92528e47a21598be7f350aea5b1a04
> a38dc7e581bb22b3add7092952d6b4915ce8ba3c
> 66c9b5ba2c335a34e42ce194944b529c391df9de
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...
cmd_ok <<'...', { at => '2008-10-07T03:37:00Z' };
$ ./clk entry-search --between 3m now
> 66c9b5ba2c335a34e42ce194944b529c391df9de
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...

# try a couple error conditions
cmd_ok <<'...';
$ ./clk entry-search --period
! --period requires an argument
? 255
...
cmd_ok <<'...';
$ ./clk entry-search --period this-is-not-a-period
! 'this-is-not-a-period' is not a valid period
? 255
...
