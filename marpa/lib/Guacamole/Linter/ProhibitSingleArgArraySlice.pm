package Guacamole::Linter::ProhibitSingleArgArraySlice;
use strict;
use warnings;
use Ref::Util qw< is_arrayref is_hashref is_ref >;
use standard;
use experimental qw< postderef signatures >;
use Guacamole;
use DDP;
use Data::Visitor::Tiny;

sub lint ( $class, $string ) {
    my @tokens = "Guacamole"->parse($string);
    visit( \@tokens, sub ( $key, $value, $ctx ) {
        $value->$* eq 'DerefVariableSlice'
            and return $ctx->{'deref'} = 1;

        $ctx->{'deref'} && $value->$* eq '@['
            and return $ctx->{'array'} = 1;

        delete $ctx->{'array'}
            or return;

        my @args = $value->$*->@*;
        $args[0] eq 'ExprValue' && @args == 2 && is_single_value( $args[1] )
            and die 'Problem!';
    });

    return 1;
};

sub is_single_value ($arg) {
    $arg->[0] eq 'Value' && $arg->[1][0] eq 'Literal'
        and return 1;

    $arg->[0] eq 'Value'
        && $arg->[1][0] eq 'NonLiteral'
        && $arg->[1][1][0] ne 'SubCall'
        and return 1;

    return 0;
};

1;
