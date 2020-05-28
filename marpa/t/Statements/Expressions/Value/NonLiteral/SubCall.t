use strict;
use warnings;
use Guacamole::Test;
use Test::More;

parses('a()');
parses('a::b()');
parsent('a::()');

done_testing();
