package Guacamole::Linter::Utils;
# ABSTRACT: Utilities for Guacamole Linter

use strict;
use warnings;
use experimental qw< postderef signatures >;
use standard;
use Exporter qw< import >;

our @EXPORT_OK = qw< trail >;

sub trail ( $arg, @elements ) {
    my $cur = $arg;

    while ( my $elem = shift @elements ) {
        $cur->{'name'} eq $elem
            or return;

        # there is more to traverse
        # so we bump the current to next child
        if (@elements) {
            $cur->{'children'} && $cur->{'children'}->@* == 1
                or return;

            $cur = $cur->{'children'}[0];
        }
    }

    return $cur;
}


1;
