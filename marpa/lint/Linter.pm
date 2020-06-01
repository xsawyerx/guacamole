package Guacamole::Linter;
use strict;
use warnings;
use standard;
use experimental qw< postderef signatures >;
use Module::Runtime qw< use_module >;
use Test::More;
use Exporter qw< import >;
use DDP;
use Guacamole::Linter::Element;

our @EXPORT = qw< lint_fail lint_success done_testing >;

sub _generate_iterator ($struct) {
    'Guacamole::Linter::Element'->new(
        'struct' => $struct
    );
};

sub run ( $subclass, $string ) {
    my $class = __PACKAGE__ . "::$subclass";
    use_module($class);

    my $struct = 'Guacamole'->parse($string);
    my $iter   = _generate_iterator($struct);

    return $iter;
}

sub lint_fail ( $subclass, $string, $desc = '' ) {
    my $iter = run( $subclass, $string );

    eval {
        $class->lint($iter);
        1;
    } or do {
        my $error = 'Cannot parse';
        ok($error, $desc || "[$string] failed: $error");
        return;
    };

    ok( 0, $desc || "[$string] did not fail" );
};

sub lint_success ( $subclass, $string ) {
    my $iter = run( $subclass, $string );

    ok(
        $class->lint($iter),
        "[$string] lints well"
    );
};

1;
