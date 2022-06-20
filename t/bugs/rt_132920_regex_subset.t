use strict;
use warnings;
use Guacamole::Test;

parses('
    map { s/\\\// } @_;
1;
');

done_testing();
