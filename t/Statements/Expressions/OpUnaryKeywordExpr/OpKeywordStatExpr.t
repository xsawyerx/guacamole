use strict;
use warnings;
use Guacamole::Test;

parses('my $perm = (stat $fh)[2] & 07777;');

done_testing();
