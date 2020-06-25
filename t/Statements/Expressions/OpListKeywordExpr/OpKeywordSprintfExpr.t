use strict;
use warnings;
use Guacamole::Test;

parses('sprintf $foo;');
parses('sprintf "hello";');
parses('sprintf $format, $bar, $baz;');
parsent('sprintf {$fh} $foo;');
parsent('sprintf {$fh} $foo, bar();');
parsent('sprintf STDOUT $foo, bar();');
parsent('sprintf STDERR $foo, bar();');
parsent('sprintf STDERR;');

parses('sprintf "%s", $foo;');
parses('sprintf "%s", "hello";');
parses('sprintf $foo;');
parses('sprintf $foo, bar();');

parsent('sprintf OTHER_THING $foo;');
parsent('sprintf foo;');
parses('sprintf foo();');

done_testing();
