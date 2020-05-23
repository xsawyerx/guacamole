package Guacamole::Linter;
use strict;
use warnings;
use standard;
use experimental qw< postderef signatures >;
use Module::Runtime qw< use_module >;
use Test::More;
use Exporter qw< import >;
use DDP;

our @EXPORT = qw< lint_fail lint_success done_testing >;

sub lint_fail ( $subclass, $string ) {
    my $class = __PACKAGE__ . "::$subclass";
    use_module($class);

    eval {
        $class->lint($string);
        1;
    } or do {
        my $error = 'Cannot parse';
        ok($error, "[$string] failed: $error");
        return;
    };

    ok( 0, "[$string] did not fail" );
};

sub lint_success ( $subclass, $string ) {
    my $class = __PACKAGE__ . "::$subclass";
    use_module($class);

    ok(
        $class->lint($string),
        "[$string] lints well"
    );
};

1;
