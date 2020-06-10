package Guacamole::Linter::Test;
# Testing framework for Guacamole Linter

use Moose;
use standard;
use experimental qw< postderef signatures >;
use Test::More;
use Guacamole::Linter;

has(
    'linter' => (
        'is'      => 'ro',
        'isa'     => 'Guacamole::Linter',
        'builder' => '_build_linter',
        'handles' => [ qw< run > ],
    )
);

sub _build_linter ($self) {
    return Guacamole::Linter->new();
}

sub lint_ok ( $self, $policy_name, $string, $desc = '' ) {
    local $Test::Builder::Level = $Test::Builder::Level + 1;

    my $error;
    eval {
        $self->run( $policy_name, $string );
        1;
    } or do {
        $error = $@ // 'Zombie error';
    };

    ok($error, $desc || "Lint detected issue: $string");
}

sub lint_nok ( $self, $policy_name, $string, $desc = '' ) {
    local $Test::Builder::Level = $Test::Builder::Level + 1;

    my $error;
    eval {
        $self->run( $policy_name, $string );
        1;
    } or do {
        $error = $@ // 'Zombie error';
    };

    ok( !$error, $desc || "Lint did not detect issue: $string" );
}

__PACKAGE__->meta()->make_immutable();
no Moose;

1;
