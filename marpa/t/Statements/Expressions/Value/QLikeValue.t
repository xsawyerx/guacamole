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
    # This condition helps test with modifiers too
    my $delimiters = ( $function eq 'm' || $function eq 'qr' ) ? 'xms' : '';

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

        # q-like value with space will fail
        # this includes s / m / y / tr
        parsent($bad_string);
    }
}

parses('$foo =~ /foo/');
parses('$foo =~ /foo/xms');
parses('$foo =~ //');
parses('$foo =~ //xms');

# Should this work?
#parses('
#s {foo}  # Replace foo
#  {bar}  # with bar.
#');

parses('$foo =~ /foo/');
parses('$foo =~ /foo/xms');
parses('$foo =~ //');
parses('$foo =~ //xms');

parses('tr/h-k/H-K/');
parses('y/h-k/H-K/');
parses('$foo =~ s/foo/bar/');
parses('$foo =~ s/foo//');
parses('$foo =~ s///');

parses('tr{h-k}{H-K}');
parses('y{h-k}{H-K}');
parses('$foo =~ s{foo}{bar}');
parses('$foo =~ s{foo}{}');
parses('$foo =~ s{}{}');
parses('$foo =~ s{}{}g');

parses('`foo`');
parses('``');

# make sure we didn't screw up non q-like subroutine parsing
parses('z()');
parses('qz()');
parses('qxx()');
parses('qX()');
parses('QX()');
parses('Q()');
parses('S()');
parses('Sr()');
parses('sr()');
parses('M()');
parses('mr()');
parses('T()');
parses('TR()');
parses('Tr()');
parses('tR()');
parses('trz()');
parses('rtz()'); # this somehow raised a bug, keeping it

done_testing;
