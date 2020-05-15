use strict;
use warnings;

use Guacamole::Test;


parses('$foo');
parses('$#foo::bar');
parses('%$foo::bar::baz');
parses('$#$foo');
parses('@$$bar');

parsent('@@foo');
parsent('@$#foo');
parsent('@%foo');
parsent('@@$foo');
parsent('$#@foo');

parses('&foo()');
parsent('$foo()');
parses('&$foo()');
parsent('$$foo()');

done_testing;
