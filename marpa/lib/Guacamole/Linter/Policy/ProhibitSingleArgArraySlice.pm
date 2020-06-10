package Guacamole::Linter::Policy::ProhibitSingleArgArraySlice;
# ABSTRACT: Test policy

use Moose;
use standard;
use experimental qw< postderef signatures >;
use Data::Visitor::Tiny;
use Guacamole::Linter::Utils qw< trail >;

with( qw< Guacamole::Linter::Role::Policy > );

sub lint ( $class, $struct ) {
    visit( $struct, sub ( $key, $value, $ctx ) {
        # Find a post dereference slice
        ref $value->$* eq 'HASH'
            && $value->$*->{'type'} ne ':bare'
            && $value->$*->{'name'} eq 'DerefVariableSlice'
            or return;

        # Find the correct opening
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
    my $f = trail( $arg, qw< ExprValueR Value Literal > )
        and return 1;


    if ( my $nonliteral = trail( $arg, qw< ExprValueR Value NonLiteral > ) ) {
        $nonliteral->{'children'}[0]{'name'} eq 'SubCall'
            or return return 1;
    }

    if ( my $keyword = trail( $arg, qw< ExprValueR OpUnaryKeywordExpr > ) ) {
        $keyword->{'children'}[0]{'name'} eq 'OpKeywordScalarExpr'
            and return 1;
    }

    return 0;
}

1;
