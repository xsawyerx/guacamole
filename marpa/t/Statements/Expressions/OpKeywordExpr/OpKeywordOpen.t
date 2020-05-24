use strict;
use warnings;
use Guacamole::Test;

parses('open($fh, "<", "foo");');
parses('open $fh, "<", "foo";');
parses('open(my $fh, "<", "foo");');
parses('open my $fh, "<", "foo";');
parses('my $res = open my $fh, "<", "foo";');

done_testing();
