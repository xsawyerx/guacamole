use strict;
use warnings;
use Guacamole::Test;

parses('sort $a + $b');
parses('sort { $a <=> $b } @foo');
parses('sort $foo');
parses('sort $foo @foo');
parses('sort @foo');

done_testing();
