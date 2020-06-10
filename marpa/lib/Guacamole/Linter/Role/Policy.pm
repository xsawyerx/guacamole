package Guacamole::Linter::Role::Policy;
# ABSTRACT: Role for Guacamole Linter Policies

use Moose::Role;
use experimental qw< postderef signatures >;
use standard;

requires( qw< lint > );

no Moose::Role;

1;
