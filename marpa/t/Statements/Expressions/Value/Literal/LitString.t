use strict;
use warnings;
use Test::More;
use Guacamole::Test;

parses(q{"Foo"});
parses(q{"Fo#o"});

TODO: {
    todo_skip 'Strings beginning with #' => 2;
    parses(q{"#Foo"});
    parses(q{'#Foo'});
}

done_testing();
