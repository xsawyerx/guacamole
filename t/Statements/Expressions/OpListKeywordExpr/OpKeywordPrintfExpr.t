use strict;
use warnings;
use Guacamole::Test;

parses('printf $foo;');
parses('printf "hello";');
parses('printf $foo, $bar, $baz;');
parses('printf {$fh} $foo;');
parses('printf {$fh} $foo, bar();');
parses('printf STDOUT $foo, bar();');
parses('printf STDERR $foo, bar();');
parses('printf STDERR');

parsent('printf OTHER_THING $foo;');
parsent('printf foo;');
parses('printf foo();');

done_testing();
