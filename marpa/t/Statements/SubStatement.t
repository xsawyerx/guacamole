use strict;
use warnings;
use Guacamole::Test;

parses('sub foo');
parses('sub foo;');
parses('sub foo () {1}');
parses('sub foo :attr {1}');
parses('sub foo : attr {1}');
parses('sub foo : attra :attrb {1}');
parses('sub foo :attr(foo) {1}');
parses('sub foo : attr(foo) {1}');
parses('sub foo ( $left, $right ) {1}');
parses('sub foo ($first, $bar, $third){1}');
parses('sub foo ($left, $right = 0) {1}');
parses('sub foo ($thing, $id = $auto_id++) {1}');
parses('sub foo :prototype($) {1}');
parses('sub foo :prototype($@\@) ($thing, $id = $auto_id++) {1}');

parses('Foo->method();');

done_testing();
