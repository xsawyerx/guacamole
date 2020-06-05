package Guacamole::AST;
use strict;
use warnings;

use Exporter "import";
use Data::Dumper;

our @EXPORT_OK = qw/traverse cleanup/;

# Remove useless text nodes.
my %_clean_paren_type = (
    "ParenExpr" => [ "(", ")" ],
    "ArrayElem" => [ "[", "]" ],
    "HashElem"  => [ "{", "}" ],
    "LitArray"  => [ "[", "]" ],
    "LitHash"   => [ "{", "}" ],
    "Block"     => [ "{", "}" ],
);

sub clean_paren {
    my $node = shift;

    $node->{'type'} eq ':bare'
        and return $node;

    my $paren = $_clean_paren_type{ $node->{'name'} };
    return $node unless defined $paren;

    die "bad node structure: " . Dumper($node)
        unless shift @{ $node->{'children'} } eq $paren->[0]
            && pop   @{ $node->{'children'} } eq $paren->[1];

    return $node;
}

sub traverse {
    my ($node, $visitor) = @_;

    if (!ref $visitor) {
        $visitor = __PACKAGE__->can($visitor) || die "Bad visitor: $visitor";
    }

    my $node2 = $visitor->($node);
    return ref $node2 ? [ map traverse($_, $visitor), @{ $node2->{'children'} } ] : $node2;
}

# Apply all cleanups to the tree.
sub cleanup {
    my $node = shift;

    traverse($node, "clean_paren");

    return $node;
}

1;
