use strict;
use warnings;
use Test::More tests => 12;
use App::Clk::Test;

clk_setup_test({ make_data => 1 });

# output the content
cmd_ok <<'...';
$ ./clk entry-search --output content --between 1223350420 1223350590
> id 241af75967962f66faad9df1d4bf2780c6da91c5
> content 56
> time: 2008-10-07T03:33:50Z
> identity: tester@example.org
> id 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> content 56
> time: 2008-10-07T03:33:59Z
> identity: tester@example.org
> id cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> content 56
> time: 2008-10-07T03:36:06Z
> identity: tester@example.org
> id 0422f139d6becb2a1451b3f2228f4bf633098900
> content 56
> time: 2008-10-07T03:36:30Z
> identity: tester@example.org
...

# output the content and the duration
cmd_ok <<'...', { at => '2008-10-08T03:40:00Z' };
$ ./clk entry-search --output content,duration --between 1223350420 1224000000
> id 241af75967962f66faad9df1d4bf2780c6da91c5
> duration 9
> content 56
> time: 2008-10-07T03:33:50Z
> identity: tester@example.org
> id 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> duration 127
> content 56
> time: 2008-10-07T03:33:59Z
> identity: tester@example.org
> id cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> duration 24
> content 56
> time: 2008-10-07T03:36:06Z
> identity: tester@example.org
> id 0422f139d6becb2a1451b3f2228f4bf633098900
> duration 86376
> content 56
> time: 2008-10-07T03:36:30Z
> identity: tester@example.org
> id dbe3e01151ad7734842e353b9fea5668f1c6e75f
> duration 234
> content 56
> time: 2008-10-08T03:36:06Z
> identity: tester@example.org
...

# output just the durations
cmd_ok <<'...';
$ ./clk entry-search --output duration --between 1223350420 1224000000
> id 241af75967962f66faad9df1d4bf2780c6da91c5
> duration 9
> id 8d9fd3f3f02147a205a789fd23513fb53ae5fb50
> duration 127
> id cf0b2824e2c9f3a4736c0f8a6e0bb5e208f67d98
> duration 24
> id 0422f139d6becb2a1451b3f2228f4bf633098900
> duration 86376
> id dbe3e01151ad7734842e353b9fea5668f1c6e75f
> duration 234
...

# output with an invalid specifier
cmd_ok <<'...';
$ ./clk entry-search --output no-such-thing
! Invalid output kind: no-such-thing
? 255
...
