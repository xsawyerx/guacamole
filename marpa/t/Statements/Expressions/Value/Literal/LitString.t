use strict;
use warnings;
use Guacamole::Test;

parses(q{"Foo"});
parses(q{"Fo#o"});
parses(q{"#Foo"});
parses(q{'#Foo'});

done_testing();
