use strict;
use warnings;
use Guacamole::Test;

parses('while ( my ( $foo, $bar ) = splice @foo, 0, 2 ) {1}');
parses('while ( my $foo = splice @foo, 0, 1 ) {1}');

done_testing();
