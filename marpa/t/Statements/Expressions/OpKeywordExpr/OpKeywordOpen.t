use strict;
use warnings;
use Guacamole::Test;

parses('open(my $fh, "<", "foo");');

done_testing();
