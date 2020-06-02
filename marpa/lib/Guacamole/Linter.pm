package Guacamole::Linter;
use strict;
use warnings;
use standard;
use experimental qw< postderef signatures >;
use Exporter qw< import >;
use Test::More;
use Module::Runtime qw< use_module >;
use Guacamole::Linter::Iterator;

our @EXPORT = qw< lint_fail lint_success done_testing >;

sub run ( $subclass, $string ) {
    # Forget about this for now
    #my $class  = __PACKAGE__ . '::' . $subclass;
    #use_module($class);

    my $struct = [ Guacamole->parse($string) ];
    my $iter   = Guacamole::Linter::Iterator->new(
        'struct' => $struct
    );

    return $iter;
}

# Forget these for now...
#sub lint_fail ( $subclass, $string, $desc = '' ) {
#    my $iter = run( $subclass, $string );
#
#    eval {
#        #$class->lint($iter);
#        1;
#    } or do {
#        my $error = 'Cannot parse';
#        ok($error, $desc || "[$string] failed: $error");
#        return;
#    };
#
#    ok( 0, $desc || "[$string] did not fail" );
#};
#
#sub lint_success ( $subclass, $string ) {
#    my $iter = run( $subclass, $string );
#
#    ok(
#        $class->lint($iter),
#        "[$string] lints well"
#    );
#};

1;
