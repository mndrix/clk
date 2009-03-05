use strict;
use warnings;
use Test::More tests => 15;
use App::Clk::Test;

clk_setup_test({ make_data => 1 });

cmd_ok <<'...';
$ ./clk entry-search --tail 1
> dbe3e01151ad7734842e353b9fea5668f1c6e75f
...

cmd_ok <<'...';
$ ./clk entry-search --tail 2
> 0422f139d6becb2a1451b3f2228f4bf633098900
> dbe3e01151ad7734842e353b9fea5668f1c6e75f
...

cmd_ok <<'...';
$ ./clk entry-search --tail 4
> 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
> dbe3e01151ad7734842e353b9fea5668f1c6e75f
...

cmd_ok <<'...';
$ ./clk entry-search --tail 7
> 489931bdd0314f6bd84fe735326a9be8fe8c721c
> 053b5c718fb4935dbea789e49600e9abe043a1ea
> 241af75967962f66faad9df1d4bf2780c6da91c5
> 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
> dbe3e01151ad7734842e353b9fea5668f1c6e75f
...

cmd_ok <<'...';
$ ./clk entry-search --tail 90
> 489931bdd0314f6bd84fe735326a9be8fe8c721c
> 053b5c718fb4935dbea789e49600e9abe043a1ea
> 241af75967962f66faad9df1d4bf2780c6da91c5
> 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> 0422f139d6becb2a1451b3f2228f4bf633098900
> dbe3e01151ad7734842e353b9fea5668f1c6e75f
...
