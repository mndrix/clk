use strict;
use warnings;
use Test::More tests => 27;
use App::Clk::Test;

clk_setup_test({ make_data => 1 });

# search for entries between two epoch times (in a single timeline)
cmd_ok <<'...';
$ ./clk entry-search --between 1223350580 1223350595
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...
cmd_ok <<'...';
$ ./clk entry-search --between 1223350500 1223350595
> 66c9b5ba2c335a34e42ce194944b529c391df9de
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...

# search for entries between two epoch times (in two timelines)
cmd_ok <<'...';
$ ./clk entry-search --between 1223350420 1223350590
> 235843b6de92528e47a21598be7f350aea5b1a04
> a38dc7e581bb22b3add7092952d6b4915ce8ba3c
> 66c9b5ba2c335a34e42ce194944b529c391df9de
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...

# search for entries between two epoch times (in three timelines)
cmd_ok <<'...';
$ ./clk entry-search --between 1223350315 1223350590
> 951cf39364677d67a28c519a08578184c75d5f52
> 235843b6de92528e47a21598be7f350aea5b1a04
> a38dc7e581bb22b3add7092952d6b4915ce8ba3c
> 66c9b5ba2c335a34e42ce194944b529c391df9de
> 5bfe463af508c92b05d5c3c62379e7f8a73317f2
...

# search for some entries with a non-epoch instant
cmd_ok <<'...', { at => '2008-10-07T03:37:00Z' };
$ ./clk entry-search --between 5m now
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

# try some error conditions
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
cmd_ok <<'...';
$ ./clk entry-search --between 1223350580 not-an-instant
! 'not-an-instant' is not a valid instant
? 255
...
