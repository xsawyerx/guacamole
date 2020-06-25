package pit;

use strict;
use warnings;
use v5.18.0;

use XSLoader;

XSLoader::load("NoPit", "0.1");

sub import {
    $^H{"pit/remove"} = undef;
}

sub unimport {
    $^H{"pit/remove"} = 1;
}

1;
