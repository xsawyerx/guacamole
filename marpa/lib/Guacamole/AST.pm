package Guacamole::AST;
use strict;
use warnings;

use Exporter "import";
use Data::Dumper;

our @EXPORT_OK = qw/traverse cleanup/;

# Translate a subtree of nested comma expressions into a List.
sub fold_comma {
    my $node = shift;
    return $node unless ref $node;
    
    my ($head, $l, $op, $r) = @$node;
    return $node unless $head eq "Expression" && defined $op && ($op eq "," || $op eq "=>");

    return [ List => _fold_comma_inner($node) ];

}

sub _fold_comma_inner {
    my $node = shift;
    return ($node) unless ref $node;

    my ($head, $l, $op, $r) = @$node;
    return ($node) unless $head eq "Expression" && defined $op && ($op eq "," || $op eq "=>");

    my @el = _fold_comma_inner($l);
    my @er = _fold_comma_inner($r);

    return (@el, @er);
}

# Combine nested Ident nodes.
sub fold_ident {
    my $node = shift;

    return $node unless ref $node;

    my ($head, $name, $sep, $rest) = @$node;
    return $node unless $head eq "Ident";

    my @path = ($name);
    while (defined $rest && ref $rest) {
        ($head, $name, $sep, $rest) = @$rest;
        push @path, $name;
    }

    return [ Ident => @path ];
}

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

# Just for lulz
sub fold_constants {
    my $node = shift;
    return $node unless ref $node;

    my ($head, $l, $op, $r) = @$node;
    return $node unless $head eq "Expression" && defined $op && ($op eq "+" || $op eq "-" || $op eq "*" || $op eq "/");

    # Fold our arguments first.
    my $fl = traverse($l, "fold_constants");
    my $fr = traverse($r, "fold_constants");

    my $ll = _extract_literal($fl) // return $node;
    my $lr = _extract_literal($fr) // return $node;

    my $expr = "$ll $op $lr";
    my $result = eval "$expr" // die "Failed to fold expression: $expr: $@";

    return [ Expression => [ Value => [ Literal => $result ]]];
}

sub _extract_literal {
    my $node = shift;

    # We don't need no pattern matching...
    return ref $node && $node->[0] eq "Expression" && ref ($node = $node->[1]) 
                     && $node->[0] eq "Value" && ref ($node = $node->[1]) 
                     && $node->[0] eq "Literal" && !ref ($node = $node->[1])
        ? $node
        : undef;
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

    $node = traverse($node, "fold_comma");
    $node = traverse($node, "fold_ident");
    $node = traverse($node, "clean_paren");

    return $node;
}


1;
