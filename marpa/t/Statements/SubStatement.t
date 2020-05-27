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

# make sure we didn't screw up non q-like subroutines
parses('sub z {1}');
parses('sub qz {1}');
parses('sub qxx {1}');
parses('sub qX {1}');
parses('sub QX {1}');
parses('sub Q {1}');
parses('sub S {1}');
parses('sub Sr {1}');
parses('sub sr {1}');
parses('sub M {1}');
parses('sub mr {1}');
parses('sub T {1}');
parses('sub TR {1}');
parses('sub Tr {1}');
parses('sub tR {1}');
parses('sub trz {1}');
parses('sub rtz {1}'); # this somehow raised a bug, keeping it

done_testing();
