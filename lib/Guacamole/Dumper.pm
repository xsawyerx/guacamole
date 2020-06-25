package Guacamole::Dumper;
use strict;
use warnings;

use List::Util qw/any sum/;
use Exporter "import";

our @EXPORT_OK = qw/dump_tree/;

sub dump_tree {
    my ($tree, $offset) = @_;
    return join "", map "$_\n", _dump_tree_inner($tree, "", $offset);
}

sub _dump_tree_inner {
    my ($tree, $indent, $offset) = @_;
    $indent //= "";
    $offset //= "  ";

    ref $tree eq 'HASH'
        or die "Bad token object: $tree";

    my $head = $tree->{
        $tree->{'type'} eq ':bare'
        ? 'value'
        : 'name'
    };

    my @tail = $tree->{'type'} eq ':bare'
             ? ()
             : @{ $tree->{'children'} };

    if ( any { ref $_ } @tail ) {
        my @rest = map { ref $_ ? _dump_tree_inner($_, "$indent$offset", $offset) : "$indent$offset'$_'" } @tail;

        my @clean = map { s/^\s+//r } @rest;
        if (sum(map length, @clean) < 40) {
            my @items = ($head, @clean);
            return ("$indent(@items)");
        }

        $rest[-1] .= ")";
        return ("$indent($head", @rest);
    } else {
        my @tailq = map "'$_'", @tail;
        my @items = ($head, @tailq);
        return ("$indent(@items)");
    }
}

1;
