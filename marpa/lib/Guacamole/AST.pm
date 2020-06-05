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
    "HashElem" => [ "{", "}" ],
    "LitArray" => [ "[", "]" ],
    "LitHash" => [ "{", "}" ],
    "Block" => [ "{", "}" ],
);

sub clean_paren {
    my $node = shift;

    return $node unless ref $node;
    my ($head, @list) = @$node;

    my $paren = $_clean_paren_type{$head};
    return $node unless defined $paren;

    die "bad $head structure" . Dumper($node)
        unless shift @list eq $paren->[0] && pop @list eq $paren->[1];
    
    return [ $head, @list ];
}







}

sub traverse {
    my ($node, $visitor) = @_;

    if (!ref $visitor) {
        $visitor = __PACKAGE__->can($visitor) || die "Bad visitor: $visitor";
    }

    my $node2 = $visitor->($node);
    return ref $node2 ? [ map traverse($_, $visitor), @$node2 ] : $node2;
}

# Apply all cleanups to the tree.
sub cleanup {
    my $node = shift;

    traverse($node, "clean_paren");

    return $node;
}


1;
