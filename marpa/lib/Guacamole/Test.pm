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
    parses_as
    parsent
    done_testing
);

sub parses {
    my ($text) = @_;

    local $Test::Builder::Level += 1;

    my @trees;
    eval { @trees = Guacamole->parse($text); }
    or do { diag($@); };

    # debugging
    # use DDP;
    # my @dumped_trees = map dump_tree( cleanup($_) ), @trees;
    # p @dumped_trees;

    is( scalar(@trees), 1, "'$text': parsed unambiguously" );
    return \@trees;
}

sub parses_as {
    my ( $text, $user_trees ) = @_;

    my $trees             = parses($text);
    my @dumped_trees      = map dump_tree( cleanup($_) ), @{$trees};
    my @dumped_user_trees = map dump_tree( cleanup($_) ), @{$user_trees};

    is_deeply(
        \@dumped_user_trees,
        \@dumped_trees,
        "'$text': parsed exactly as expected",
    );
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
        diag( dump_tree($ast) );
    }
}

1;
