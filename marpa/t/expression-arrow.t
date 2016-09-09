use strict;
use warnings;

use Guacamole::Test;

parses('foo()->(foo())');
parses('foo()->BAR::baz()');
parses('foo()->$BAR::baz()');
parses('foo()->[1]');
parses('foo()->{1}');

done_testing;
