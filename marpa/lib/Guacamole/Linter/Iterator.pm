package Guacamole::Linter::Iterator;
use Moose;
use Data::Visitor::Tiny;
use Ref::Util qw< is_ref is_arrayref >;
use Clone qw< clone >;
use experimental qw< postderef signatures >;
use standard;

has(
    'struct',
    'is'       => 'ro',
    'isa'      => 'ArrayRef',
    'required' => 1
);

has(
    'items',
    'is'      => 'ro',
    'isa'     => 'ArrayRef',
    'lazy'    => 1,
    'builder' => '_build_items'
);

has(
    'iterator',
    'is'      => 'ro',
    'isa'     => 'Iterator',
    'lazy'    => 1,
    'builder' => '_build_iterator'
);

sub BUILD ( $self, $args ) { return $self->items(); }

sub _process_elem ($struct) {
    foreach my $elem ( $struct->@* ) {
        if ( is_arrayref($elem) ) {
            # first go full depth
            _process_elem($elem);

            if ( $elem->@* > 3 ) {
                my @children = $elem->@[ 1 .. $elem->$#* ];
                $elem = {
                    'name'     => $elem->[0],
                    'children' => \@children
                };
            } elsif ( $elem->@* == 2 ) {
                $elem = { 'name' => $elem->[0], 'value' => $elem->[1] };
            } elsif ( $elem->@* == 1 ) {
                $elem = { ':bare' => $elem->[0] };
            }

        }
    }
}

sub _build_items ($self) {
    my $struct = clone( $self->struct() );

    _process_elem($struct);

    return $struct;
}

sub next ($self) {
    1;
}

no Moose;
__PACKAGE__->meta()->make_immutable();

1;

