use strict;
use warnings;
use Test::More tests => 1;

TODO: {
    local $TODO = 'implement some real tests';
    ok(0);
}

# TODO allow for inference hooks so that people can customize
#      the way that an entry is generated based the "in"
#      commands arguments.  Perhaps this is only a single hook.
#      If someone wants to have multiple layers of inference
#      hooks, they can have the one true hook call out to those
#      smaller hooks.
