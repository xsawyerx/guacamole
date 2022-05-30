#!/usr/bin/env perl

use strict;
use warnings;
use utf8;

use Guacamole::Test;

parses( 'binmode $fh;' );
parses( 'binmode( $fh );' );
parses( 'binmode STDIN;' );
parses( 'binmode( STDIN );' );
parses( 'binmode $fh, ":utf8";' );
parses( 'binmode( $fh, ":utf8" );' );
parses( 'binmode STDIN, ":utf8";' );
parses( 'binmode( STDIN, ":utf8" );' );
parses( 'binmode( STDIN, ":utf8" );' );

done_testing();
