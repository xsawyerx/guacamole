use strict;
use warnings;
use Test::More;
use Test::Fatal qw< exception >;
use Guacamole::Test;

parses('$foo->$*');
parses('$foo->@*');
parses('$foo->%*');
parses('$foo->&*');
parses('$foo->**');
parses('$foo->$#');

parses('$foo->@[ 0, 3 ]');
parses('$foo->%[ 0, 3 ]');

parses('$foo->@{ 0, 3 }');
parses('$foo->%{ 0, 3 }');

ok(
    exception( sub { parse_fail('$foo->% { "foo", "bar" }') } ),
    'Failed to parse: $foo->% { "foo", "bar" }',
);

done_testing();
