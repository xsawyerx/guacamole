use strict;
use warnings;
use Test::More;
use Guacamole::Test;


parses('$foo');
parses('$#foo::bar');
parses('%$foo::bar::baz');
parses('$#$foo');
parses('@$$bar');

parsent('@@foo');

TODO: {
    todo_skip 'Surface issues with unacceptable deref' => 1;
    parsent('@$#foo');
}

parsent('@%foo');
parsent('@@$foo');
parsent('$#@foo');

parses('&foo()');
parsent('$foo()');
parses('&$foo()');
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
parses("stat[2]"); # stat anonarray

parsent("5[1]");
parsent('$x->y[1]');

done_testing;
