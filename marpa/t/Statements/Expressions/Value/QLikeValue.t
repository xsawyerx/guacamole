use strict;
use warnings;
use experimental qw< postderef >;

use Test::More;
use Test::Fatal qw< exception >;
use Guacamole::Test;

# qw is using spaces in between - that's different
# qr can have regex modifiers - that's different
# here we're just testing the delimiters
my @q_functions = ( 'q', 'qq', 'qw', 'qx', 'qr', 'm' );

my @delimiters = (
    [ '(', ')' ], # q(...) | q()
    [ '{', '}' ], # q{...} | q{}
    [ '<', '>' ], # q<...> | q<>
    [ '/', '/' ], # q/.../ | q//
    [ '!', '!' ], # q!...! | q!!
);

foreach my $function (@q_functions) {
    my $has_delimiters = $function eq 'm' || $function eq 'qr';
    my $delimiters     = $has_delimiters ? 'xms' : '';

    foreach my $delimiter_set (@delimiters) {
        my $simple_string = sprintf 'say %s%s$foo%s%s',
                            $function,
                            $delimiter_set->@[ 0, 1 ],
                            $delimiters;

        parses($simple_string);

        my $escaped_string = sprintf 'say %s%s \\%s $foo \\%s %s%s',
                             $function,
                             $delimiter_set->@[ 0, 0, 1, 1 ],
                             $delimiters;

        parses($escaped_string);

        # and one without delimiters
        my $emptystring = sprintf 'say %s%s%s%s', $function,
                          $delimiter_set->@[ 0, 1 ],
                          $delimiters;

        parses($emptystring);

        my $bad_string = sprintf 'say %s %s%s', $function,
                         $delimiter_set->@[ 0, 1 ];

        like(
            exception( sub { parse_fail($bad_string) } ),
            qr/Error \s in \s SLIF \s parse/xms,
            "Refuse to parse: $bad_string",
        );
    }
}

done_testing;
