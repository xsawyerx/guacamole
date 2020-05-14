use strict;
use warnings;
use experimental qw< postderef >;

use Guacamole::Test;

# LitNumber
parses('-4');
parses('4');
parses('44');
parses('44.4');
parses('-44.4');
parses('Inf');  # infinite
parses('-Inf'); # negative infinite

# LitArray
parses('[]');
parses('[ 1, 2 ]');
parses('[ 1, 2, ( 10, () ) ]');

# LitHash
parses('$x = {}');
parses('$x = { "foo" => "bar" }');

# LitString (SingleQuote)
parses(q{''});
parses(q{ 'hello' });
parses(q{ 'hello world \' foo " ' });

# InterpolString (DoubleQuote)
parses(q{""});
parses(q{ "hello" });
parses(q{ "hello world \" foo ' " });

done_testing();
