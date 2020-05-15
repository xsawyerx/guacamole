use strict;
use warnings;
use Guacamole::Test;

parses('...');
parses('...;');
parses('while (1) {...}');
parses('while (1) {...;}');
parses('while (1) { 1; ...;}');

done_testing();
