package Guacamole::Linter::Policy::ProhibitSingleArgArraySlice;
use strict;
use warnings;
use Ref::Util qw< is_arrayref is_hashref is_ref >;
use standard;
use experimental qw< postderef signatures >;
use Guacamole;
use DDP;
use Data::Visitor::Tiny;

sub lint ( $class, $struct ) {
    visit( $struct, sub ( $key, $value, $ctx ) {
        ref $value->$* eq 'HASH'
            && $value->$*->{'type'} ne ':bare'
            && $value->$*->{'name'} eq 'DerefVariableSlice'
            or return;

        # opening
        $value->$*->{'children'}[0]{'value'} eq '@['
            && $value->$*->{'children'}->@* == 3
            && $value->$*->{'children'}[-1]{'value'} eq ']'
            or return;

        if ( is_single_value( $value->$*->{'children'}[1] ) ) {
            #p $value->$*->{'children'}[1];
            die "$value->$* is not good\n";
        }
    });

    return 1;
}

sub is_single_value ($arg) {
    $arg->{'type'} eq ':bare'
        and return 1;

    # Single value is fine, if it leads to either:
    # * Literal
    # * NonLiteral that is a not a SubCall
    # * scalar ... is fine
    if ( $arg->{'name'} eq 'ExprValueR' ) {
        my @expr_children = $arg->{'children'}->@*;

        if ( $expr_children[0]{'name'} eq 'Value' ) {
            my @value_children = $expr_children[0]{'children'}->@*;
            if ( $value_children[0]{'name'} eq 'Literal' ) {
                return 1;
            } elsif ( $value_children[0]{'name'} eq 'NonLiteral' ) {
                my $nonliteral = $value_children[0]{'children'}[0];

                $nonliteral->{'name'} eq 'SubCall'
                    or return 1;
            }
        } elsif ( $expr_children[0]{'name'} eq 'OpUnaryKeywordExpr' ) {
            $expr_children[0]{'children'}[0]{'name'} eq 'OpKeywordScalarExpr'
                and return 1;
        }
    }

    return 0;
}

1;
