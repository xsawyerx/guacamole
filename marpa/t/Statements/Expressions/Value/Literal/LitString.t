use strict;
use warnings;
use Test::More;
use Guacamole::Test;

parses(q{"Foo"});
parses(q{"Fo#o"});

TODO: {
    local $TODO = 'Strings beginning with #';
    parses(q{"#Foo"});
    parses(q{'#Foo'});
}

done_testing();
