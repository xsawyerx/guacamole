use strict;
use warnings;
use experimental qw< postderef >;

use Guacamole::Test;

# qw is using spaces in between - that's different
# qr can have regex modifiers - that's different
# here we're just testing the delimiters
my @q_functions = ( 'q', 'qq', 'qw', 'qx', 'qr' );


my @delimiters = (
    [ '(', ')' ], # q(...) | q()
    [ '{', '}' ], # q{...} | q{}
    [ '<', '>' ], # q<...> | q<>
    [ '/', '/' ], # q/.../ | q//
    [ '!', '!' ], # q!...! | q!!
);

foreach my $function (@q_functions) {
    foreach my $delimiter_set (@delimiters) {
        my $string = sprintf '%s%s$foo%s',
                     $function,
                     $delimiter_set->@*;

        parses($string);

        # and one without delimiters
        my $emptystring = sprintf '%s%s%s', $function,
                          $delimiter_set->@*;
        parses($emptystring);
    }
}

done_testing;
