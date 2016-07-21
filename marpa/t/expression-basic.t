use strict;
use warnings;
use Test::More;
use Data::Dumper;

use Guacamole;
use Guacamole::Dumper qw/dump_tree/;
use Guacamole::AST qw/cleanup/;

sub try {
    my ($text) = @_;

    local $Test::Builder::Level += 1;
    my @trees = Guacamole->parse($text);
    is scalar(@trees), 1, "'$text': parsed unambiguously";

    foreach my $tree (@trees) {
        my $ast = cleanup($tree);
        print dump_tree($ast);
    }
}

try("a(1, 2, 3)");
try("a(b(1 + 2 * 5), c::d(1, e()), 4 * 5 + 1)");
try("1 * 2 * 3 * 4");

done_testing;
