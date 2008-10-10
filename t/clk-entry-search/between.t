use strict;
use warnings;
use Test::More tests => 45;
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

# create entries in a second timeline
cmd_ok <<'...';   # 2008-10-07T03:33:50Z
$ ./clk in --at 1223350430
> 235843b6de92528e47a21598be7f350aea5b1a04
...
cmd_ok <<'...';   # 2008-10-07T03:33:59Z
$ ./clk in --at 1223350439
> a38dc7e581bb22b3add7092952d6b4915ce8ba3c
...

# create entries in a third timeline
cmd_ok <<'...';   # 2008-10-07T03:31:50Z
$ ./clk in --at 1223350310
> a9aef841af1b731b65bf172072952e1cad06503b
...
cmd_ok <<'...';   # 2008-10-07T03:32:00Z
$ ./clk in --at 1223350320
> 951cf39364677d67a28c519a08578184c75d5f52
...


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

# search for some entries with a non-epoch timespec
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
