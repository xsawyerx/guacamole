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
    [ '|', '|' ], # q|...| | q||
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

foreach my $func ( qw< q qq qw qx qr s m y tr > ) {
    if ( length $func == 1 ) {
        # single q-like letter
        parses('z()');   # NonQLike single char
        parses("${func}z()");  # NonQLike double char with leading q
        parses("$func\_z()"); # NonQLike with underscore, leading q
        parses("$func\_()");  # NonQLike with underscore, leading q
        parses("_$func()");  # NonQLike with underscore, leading q
        parses( uc($func) . '()');
    }


    if ( length $func == 2 ) {
        my ( $letter1, $letter2 ) = split //xms, $func;
        # double q-like letters
        parses( sprintf '%s_()', $func );
        parses( sprintf '%s_%s()', $letter1, $letter2 );
        parses( sprintf '%s%s()', $func, $letter1 );
        parses( sprintf '%s%s()', $func, $letter2 );
        parses( sprintf '%sl()', $func );
        parses( sprintf '%sl%s()', $letter1, $letter2 );
        parses( sprintf '%s%s()', $letter1, uc $letter2 );
        parses( sprintf '%s%s()',  uc $letter1, $letter2 );
        parses( sprintf '%s()',  uc $func );
    }
}

parses('t()');
parses('r()');
parses('x()');
parses('w()');

parsent('14()'); # Numbers cannot be subnames
parsent('14_f()'); # Numbers cannot be subnames, even with underscores
parsent('14f()'); # Numbers cannot be subnames, even with letters
parses('trz()');
parses('rtz()');

done_testing;
