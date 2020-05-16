use strict;
use warnings;
use experimental qw< postderef >;

use Test::More;
use Test::Fatal qw< exception >;
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
        my $simple_string = sprintf 'say %s%s$foo%s',
                            $function,
                            $delimiter_set->@*;

        parses($simple_string);

        my $escaped_string = sprintf 'say %s%s \\%s $foo \\%s %s',
                             $function,
                             $delimiter_set->@[ 0, 0, 1, 1 ];

        parses($escaped_string);

        # and one without delimiters
        my $emptystring = sprintf 'say %s%s%s', $function,
                          $delimiter_set->@*;

        parses($emptystring);

        my $bad_string = sprintf 'say %s %s%s', $function,
                         $delimiter_set->@*;

        like(
            exception( sub { parses($bad_string) } ),
            qr/Error \s in \s SLIF \s parse/xms,
            "Refuse to parse: $bad_string",
        );
    }
}

done_testing;
