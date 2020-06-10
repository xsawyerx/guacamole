package Guacamole::Linter;
# ABSTRACT: A next generation linter

use Moose;
use standard;
use experimental qw< postderef signatures >;
use Module::Runtime qw< use_module >;
use Guacamole::Linter::Iterator;

sub run ( $self, $policy_name, $string ) {
    my $class = "Guacamole::Linter::Policy::$policy_name";
    use_module($class);
    my $policy = $class->new();

    my $struct = [ Guacamole->parse($string) ];
    $policy->lint($struct);

    return $struct;
}

__PACKAGE__->meta()->make_immutable();
no Moose;

1;
