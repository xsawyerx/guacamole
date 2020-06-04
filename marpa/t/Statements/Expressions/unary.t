use strict;
use warnings;
use Guacamole::Test;

parses('defined $x');
parses('defined $x + $y');
parses('!defined $x + $y');
parses('$x + !defined $y + $z');
parses('$x ** !defined $y << $z');

done_testing;
