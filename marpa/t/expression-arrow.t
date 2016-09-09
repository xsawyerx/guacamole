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

try('foo()->(foo())');
try('foo()->BAR::baz()');
try('foo()->$BAR::baz()');
try('foo()->[1]');
try('foo()->{1}');

done_testing;
