use strict;
use warnings;
use Guacamole::Test;

parses('$foo = { "a" => "b" }');
parses('$foo = { "a" => "b", }');
parses('$foo = [ 1, 2 ]');
parses('$foo = [ 1, 2, ]');

done_testing();
