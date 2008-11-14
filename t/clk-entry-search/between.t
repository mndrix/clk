use strict;
use warnings;
use Test::More tests => 27;
use App::Clk::Test;

clk_setup_test({ make_data => 1 });

# search for entries between two epoch times (in a single timeline)
cmd_ok <<'...';
$ ./clk entry-search --between 1223350580 1223350595
> 0422f139d6becb2a1451b3f2228f4bf633098900
...
cmd_ok <<'...';
$ ./clk entry-search --between 1223350500 1223350595
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
...

# search for entries between two epoch times (in two timelines)
cmd_ok <<'...';
$ ./clk entry-search --between 1223350420 1223350590
> 241af75967962f66faad9df1d4bf2780c6da91c5
> 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
...

# search for entries between two epoch times (in three timelines)
cmd_ok <<'...';
$ ./clk entry-search --between 1223350315 1223350590
> 053b5c718fb4935dbea789e49600e9abe043a1ea
> 241af75967962f66faad9df1d4bf2780c6da91c5
> 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
...

# search for some entries with a non-epoch instant
cmd_ok <<'...', { at => '2008-10-07T03:37:00Z' };
$ ./clk entry-search --between 5m now
> 053b5c718fb4935dbea789e49600e9abe043a1ea
> 241af75967962f66faad9df1d4bf2780c6da91c5
> 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
...
cmd_ok <<'...', { at => '2008-10-07T03:37:00Z' };
$ ./clk entry-search --between 3m now
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
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
