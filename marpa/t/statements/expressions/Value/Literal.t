use strict;
use warnings;
use experimental qw< postderef >;

use Guacamole::Test;

parses(q{ "hello" });
parses(q{ 'hello' });

parses(q{ "hello world \" foo ' " });
parses(q{ 'hello world \' foo " ' });

done_testing();
