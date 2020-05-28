use strict;
use warnings;
use Guacamole::Test;

parses('sub foo');
parses('sub foo;');
parses('sub foo () {1}');
parses('sub foo :attr {1}');
parses('sub foo : attr {1}');
parses('sub foo : attra :attrb {1}');
parses('sub foo :attr(foo) {1}');
parses('sub foo : attr(foo) {1}');
parses('sub foo ( $left, $right ) {1}');
parses('sub foo ($first, $bar, $third){1}');
parses('sub foo ($left, $right = 0) {1}');
parses('sub foo ($thing, $id = $auto_id++) {1}');
parses('sub foo :prototype($) {1}');
parses('sub foo :prototype($@\@) ($thing, $id = $auto_id++) {1}');

parses('Foo->method();');

# make sure we didn't screw up non q-like subroutines

foreach my $func ( qw< q qq qw qx qr s m y tr > ) {
    if ( length $func == 1 ) {
        # single q-like letter
        parses('sub z {1}');   # NonQLike single char
        parses("sub ${func}z {1}");  # NonQLike double char with leading q
        parses("sub $func\_z {1}"); # NonQLike with underscore, leading q
        parses("sub $func\_ {1}");  # NonQLike with underscore, leading q
        parses("sub _$func {1}");  # NonQLike with underscore, leading q
        parses( 'sub ' .  uc($func) . '{1}');
    }


    if ( length $func == 2 ) {
        my ( $letter1, $letter2 ) = split //xms, $func;
        # double q-like letters
        parses( sprintf 'sub %s_ {1}', $func );
        parses( sprintf 'sub %s_%s {1}', $letter1, $letter2 );
        parses( sprintf 'sub %s%s {1}', $func, $letter1 );
        parses( sprintf 'sub %s%s {1}', $func, $letter2 );
        parses( sprintf 'sub %sl {1}', $func );
        parses( sprintf 'sub %sl%s {1}', $letter1, $letter2 );
        parses( sprintf 'sub %s%s {1}', $letter1, uc $letter2 );
        parses( sprintf 'sub %s%s {1}',  uc $letter1, $letter2 );
        parses( sprintf 'sub %s {1}',  uc $func );
    }
}

parses('sub t {1}');
parses('sub r {1}');
parses('sub x {1}');
parses('sub w {1}');

parsent('sub 14 {1}'); # Numbers cannot be subnames
parsent('sub 14_f {1}'); # Numbers cannot be subnames, even with underscores
parsent('sub 14f {1}'); # Numbers cannot be subnames, even with letters
parses('sub trz {1}');
parses('sub rtz {1}');

done_testing();
