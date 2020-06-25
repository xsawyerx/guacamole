package Guacamole::Deparse;

use strict;
use warnings;
use parent 'Exporter';
use experimental qw< postderef signatures >;
use Guacamole;
use Data::Visitor::Tiny qw< visit >;
use Ref::Util qw< is_hashref >;

our @EXPORT_OK = qw< deparse >;

my $var_names_re = qr/^Var(Scalar|Array|Hash|Code|Glob|ArrayTop)$/;

sub deparse ($string) {
    my @results = Guacamole->parse($string);
    foreach my $result (@results) {
        my @elements;

        visit( $result, sub ( $key, $valueref, $ctx ) {
            # Fold some stuff
            if ( is_hashref( $valueref->$* ) ) {
                my $name = $valueref->$*->{'name'} || '';

                if ( $name =~ $var_names_re ) {
                    fold_var_name($valueref);
                }

                if ( $name =~ m/^(Interpol|Literal)String$/ ) {
                    fold_string($valueref);
                }
            }

            # Take lexeme as a string element
            if (
                is_hashref( $valueref->$* )
             && $valueref->$*->{'type'} eq ':bare'
            ) {
                push @elements, $valueref->$*->{'value'};
            }
        });

        print join( ' ', @elements ), "\n";
    }
}

sub fold_var_name ($valueref) {
    # Merge children
    my $sigil_elem = shift $valueref->$*->{'children'}->@*;
    my $sigil      = $sigil_elem->{'value'};

    $valueref->$*->{'children'}[0]{'children'}[0]{'value'}
        = $sigil . $valueref->$*->{'children'}[0]{'children'}[0]{'value'};
}

sub fold_string ($valueref) {
    my $value = join '', map $_->{'value'}, $valueref->$*->{'children'}->@*;

    $valueref->$*->{'children'} = [
        {
            'type'  => ':bare',
            'value' => $value,
        }
    ];
}

1;
