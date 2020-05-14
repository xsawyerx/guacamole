use strict;
use warnings;
use experimental qw< postderef >;

use Guacamole::Test;

# LitNumber

# LitArray

# LitHash

# LitString (SingleQuote)
parses(q{''});
parses(q{ 'hello' });
parses(q{ 'hello world \' foo " ' });

# InterpolString (DoubleQuote)
parses(q{""});
parses(q{ "hello" });
parses(q{ "hello world \" foo ' " });

done_testing();
