package standard;

use strict;
use warnings;
use experimental qw< signatures >;
use Guacamole;
use Path::Tiny ();

sub import ( $class ) {
    my @caller      = caller();
    my $caller_file = $caller[1];
    my $content     = Path::Tiny::path($caller_file)->slurp_utf8();

    eval { Guacamole->parse($content); }
    or do {
        print STDERR "File '$caller_file' does not pass Standard Perl.\n"
                   . "Parser says:\n"
                   . join '', map "> $_\n", split /\n/xms, $@;
    };

    return;
}


1;
