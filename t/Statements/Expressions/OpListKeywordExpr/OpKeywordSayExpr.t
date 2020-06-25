use strict;
use warnings;
use Guacamole::Test;

parses('say $foo;');
parses('say "hello";');
parses('say $foo, $bar, $baz;');
parses('say {$fh} $foo;');
parses('say {$fh} $foo, bar();');
parses('say STDOUT $foo, bar();');
parses('say STDERR $foo, bar();');
parses('say STDERR');

parsent('say OTHER_THING $foo;');
parsent('say foo;');
parses('say foo();');

done_testing();
