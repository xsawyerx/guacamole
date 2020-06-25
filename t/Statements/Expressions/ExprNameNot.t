use strict;
use warnings;
use Guacamole::Test;

parses('not defined $foo');
parses('! defined $foo && $x = 2');

done_testing();
