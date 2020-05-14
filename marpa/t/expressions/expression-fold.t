use strict;
use warnings;
use Test::More;
use Data::Dumper;

use Guacamole;
use Guacamole::Dumper qw/dump_tree/;
use Guacamole::AST qw/traverse/;

sub try {
    my ($text, $value) = @_;

    local $Test::Builder::Level += 1;
    my @trees = Guacamole->parse($text);
    is scalar(@trees), 1, "'$text': parsed unambiguously";

    my $expect = [
        Program => [ 
            StatementSeq => [
                Statement => [
                    Expression => [
                        Value => [ Literal => $value ]]]]]];

    foreach my $tree (@trees) {
        my $fold = traverse($tree, "fold_constants");
        is_deeply($fold, $expect, "'$text': is equal to '$value'");
    }
}

try("1 + 2 * 2", 5);
try("2 * 2 + 1", 5);
try("3 + 3 - 1", 5);
try("3 - 1 + 3", 5);

done_testing;
