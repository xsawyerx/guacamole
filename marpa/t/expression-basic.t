use strict;
use warnings;
use Guacamole::Test;

parses("a(1, 2, 3)");
parses("a(b(1 + 2 * 5), c::d(1, e()), 4 * 5 + 1)");
parses("1 * 2 * 3 * 4");

done_testing;
