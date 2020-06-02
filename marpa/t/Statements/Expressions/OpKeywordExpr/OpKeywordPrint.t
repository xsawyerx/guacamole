use strict;
use warnings;
use Guacamole::Test;

parses('print $foo;');
parses('print "hello";');
parses('print $foo, $bar, $baz;');
parses('print {$fh} $foo;');
parses('print {$fh} $foo, bar();');
parses('print STDOUT $foo, bar();');
parses('print STDERR $foo, bar();');
parses('print STDERR');

parsent('print OTHER_THING $foo;');
parsent('print foo;');
parses('print foo();');

done_testing();
