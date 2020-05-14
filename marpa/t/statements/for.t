use strict;
use warnings;

use Guacamole::Test;

parses('for (@foo) {1}');
parses('for $foo (@bar) {1}');
parses('for my $foo (@bar) {1}');
parses('for ($i = 0; $i < 10; $ii) {1}');
parses('for (my $i = 0; $i < 10; $ii) {1}');

parses('foreach (@foo) {1}');
parses('foreach $foo (@bar) {1}');
parses('foreach my $foo (@bar) {1}');
parses('foreach ($i = 0; $i < 10; $i) {1}');
parses('foreach (my $i = 0; $i < 10; $i) {1}');

done_testing();
