package Guacamole::Test;
use strict;
use warnings;

use Test::More;
use Data::Dumper;

use Guacamole;
use Guacamole::Dumper qw/dump_tree/;
use Guacamole::AST qw/cleanup/;

use Exporter "import";

our @EXPORT = qw(
    parses
    parsent
    done_testing
);

sub parses {
    my ($text) = @_;

    local $Test::Builder::Level += 1;

    my @trees = Guacamole->parse($text);
    is scalar(@trees), 1, "'$text': parsed unambiguously";

    foreach my $tree (@trees) {
        my $ast = cleanup($tree);
        print dump_tree($ast);
    }
}

sub parsent {
    my ($text) = @_;

    local $Test::Builder::Level += 1;

    my @trees;
    my $res = eval {
        @trees = Guacamole->parse($text);
        1;
    };
    my $err = $@;
    ok !defined $res && defined $err, "'$text': did not parse";

    foreach my $tree (@trees) {
        my $ast = cleanup($tree);
        print dump_tree($ast);
    }
}

1;
