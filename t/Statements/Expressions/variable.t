use strict;
use warnings;
use Test::More;
use Guacamole::Test;

parses('$foo');
parses('$#foo::bar');
parsent('%$foo::bar::baz');
parses('%{ $foo::bar::baz }');
parsent('$#$foo');
parses('$#{ $foo }');
parsent('@$$bar');
parses('@{ ${$bar} }');

parsent('@@foo');
parses('@$#foo'); # @$ and # foo

parsent('@%foo');
parsent('@@$foo');
parsent('$#@foo');

parses('&foo()');
parsent('$foo()');
parsent('&$foo()');
parses('$foo->()');
parsent('$$foo()');

parses('$foo[5]');
parses('@foo[1, 2]');
parses('%foo[1, 5]');
parses('$foo[bar()]');
parses('$foo["baz"]');
parses('(stat $file)[2]');
parses('$foo{1}');
parses('$foo{1, 2}');
parses('$foo{qw/foo bar/}');
parses("(stat)[2]");

parsent("5[1]");
parsent('$x->y[1]');

done_testing;
