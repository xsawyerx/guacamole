package Guacamole::Linter;
use strict;
use warnings;
use standard;
use experimental qw< postderef signatures >;
use Exporter qw< import >;
use Test::More;
use Module::Runtime qw< use_module >;
use Guacamole::Linter::Iterator;

our @EXPORT = qw< lint_ok lint_nok done_testing >;

sub run ( $subclass, $string ) {
    # Forget about this for now
    my $class  = "Guacamole::Linter::Policy::$subclass";
    use_module($class);

    my $struct = [ Guacamole->parse($string) ];
    $class->lint($struct);

    return $struct;
}

sub lint_ok ( $subclass, $string, $desc = '' ) {
    my $error;
    eval {
        run( $subclass, $string );
        1;
    } or do {
        $error = $@ // 'Zombie error';
    };

    ok($error, $desc || "Lint detected issue: $string");
}

sub lint_nok ( $subclass, $string, $desc = '' ) {
    my $error;
    eval {
        run( $subclass, $string );
        1;
    } or do {
        $error = $@ // 'Zombie error';
    };

    ok( !$error, $desc || "Lint did not detect issue: $string" );
}

1;
