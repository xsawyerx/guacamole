use strict;
use warnings;
use Guacamole::Test;

parses('readline;');
parses('readline();');
parses('readline STDIN;');
parses('readline (STDIN);');
parses('readline(STDIN);');

done_testing();
