use strict;
use warnings;
use Guacamole::Test;

parses('if ($x > 10) {1}');
parses('if ($x > 10) {1} else {2}');
parses('if ($x > 10) {1} elsif ( $x < 20 ) {2}');
parses('if ($x > 10) {1} elsif ( $x < 20 ) {3} else {3}');
parses('if ($x > 10) {1} elsif ( $x < 20 ) {2} elsif ( $x < 15 ) {3}');
parses('if ($x > 10) {1} elsif ( $x < 20 ) {3} elsif ( $x < 15 ) {3} else {4}');

parses('unless ($x > 10) {1}');

done_testing();
