use strict;
use warnings;
use Test::More tests => 1;

TODO: {
    local $TODO = 'implement some real tests';
    ok(0);
}

# TODO remove any entries which are not referenced by the timeline
#      as long as the creation time of the entry is old enough.
#      A user should be able to run 'gc' even if other commands are
#      creating entries.
# TODO pack all loose entries into a snazzy packed format
# TODO combine all timelines into a single timeline
