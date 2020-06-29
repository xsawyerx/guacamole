use strict;
use warnings;
use Guacamole::Test;
use Test::More;
use Test::Fatal;

parses('foo()->(foo())');
parses('foo()->BAR::baz()');
parses('foo()->$BAR::baz()');
parses('foo()->[1]');
parses('foo()->{1}');
parses('foo()->[1]{2}');
parses('$foo->thing()');
parses('"Foo"->thing()');
parses('Foo->thing()');

parses('Foo->thing;');

done_testing;
