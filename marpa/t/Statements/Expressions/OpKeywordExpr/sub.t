use strict;
use warnings;
use Guacamole::Test;

parses('$cb = sub  () {1}');
parses('$cb = sub  :attr {1}');
parses('$cb = sub  : attr {1}');
parses('$cb = sub  : attra :attrb {1}');
parses('$cb = sub  :attr(foo) {1}');
parses('$cb = sub  : attr(foo) {1}');
parses('$cb = sub  ( $left, $right ) {1}');
parses('$cb = sub  ($first, $bar, $third){1}');
parses('$cb = sub  ($left, $right = 0) {1}');
parses('$cb = sub  ($thing, $id = $auto_id++) {1}');
parses('$cb = sub  :prototype($) {1}');
parses('$cb = sub  :prototype($@\@) ($thing, $id = $auto_id++) {1}');
parses('$cb = sub : const {$x}');

done_testing();
