# Ref::Util::Rewriter-esque
use strict;
use warnings;
use experimental qw< postderef signatures >;
use standard;
use Guacamole;
use Data::Visitor::Tiny;
use Test::More;
use DDP;
use Clone qw< clone >;

sub rewrite_string ($string) {
    my @o_tokens = "Guacamole"->parse($string);
    my @tokens   = clone(\@o_tokens)->@*;
    my $replaced;

    # This does not yet handle recursive
    visit(@tokens, sub ( $key, $vref, $ctx ) {
        # ref ... eq ...
        # ref ... ne ...
        $vref->$* =~ m{Expr(Eq|Ne)$}xms
            and $ctx->{'collect_depth'} = $ctx->{'_depth'};

        $ctx->{'collect_depth'} && $ctx->{'_depth'} == $ctx->{'collect_depth'}
            and push $ctx->{'neq_items'}->@*, $vref;

        if ( $ctx->{'neq_items'} && $ctx->{'neq_items'}->@* == 4 ) {
            handle_neq( $ctx->{'neq_items'}->@* );
            delete $ctx->{'neq_items'};
        }

        ## ref ... =~
        #$vref->$* =~ m{ExprRegex$}xms
        #    or return handle_regex($vref);
    });

    return $replaced ? \@o_tokens : \@tokens;
};

sub handle_neq ( $ref_expr, $eq_op, $value_expr ) {
    my ( $variable, $ref_type );
    visit( $ref_expr->$*, sub ( $key, $vref, $ctx ) {
        $vref->$* eq 'Variable'
            and return $ctx->{'skip'} = 1;

        delete $ctx->{'skip'}
            or return;

        $variable = $vref->$*->[1] . $vref->$*->[2][1][1];
    });

    visit( $value_expr->$*, sub ( $key, $vref, $ctx ) {
        $vref->$* eq 'Literal'
            and return $ctx->{'skip'} = 1;

        delete $ctx->{'skip'}
            or return;

        $ref_type = $vref->$*->[1][2];
    });

    my $func
        = $ref_type eq 'HASH'  ? "is_hashref($variable)"
        : $ref_type eq 'ARRAY' ? "is_arrayref($variable)"
        :                        '';
    
    p($func);
};

sub handle_regex ($data) {
    return 1;
    visit($data, sub ( $key, $vref, $ctx ) {
        p($vref);
    });
};

sub handle_ref ($data) {
    return 1;
    visit($data, sub ( $key, $vref, $ctx ) {
        # change vref to be is_*...ref();
        p($vref);
    });
};

my @tests = (
    q{ref $foo eq 'ARRAY';}        => q{is_arrayref($foo);},
    q{ref $foo eq 'CODE';}         => q{is_coderef($foo);},
    q{ref($foo) eq 'ARRAY';}       => q{is_arrayref($foo);},
    q{ref  ($foo) eq 'ARRAY';}     => q{is_arrayref($foo);},
    q{ref($foo) or}                => q{is_ref($foo) or},
    q!if (ref($foo) eq 'ARRAY') {! => q!if (is_arrayref($foo)) {!,
    q{ref($foo) eq 'ARRAY' or}     => q{is_arrayref($foo) or},

    q{ref $foo eq 'CODE' && ref $foo eq 'ARRAY';}
        => q{is_coderef($foo) && is_arrayref($foo);},

    # a few edge cases
    q!sub { my $is_arrayref = ref $self eq 'ARRAY'; }!
        => q!sub { my $is_arrayref = is_arrayref($self); }!,

    q!sub { my $is_arrayref = ref $self eq 'ARRAY' && $x == 42; }!
       => q!sub { my $is_arrayref = is_arrayref($self) && $x == 42; }!,

    q!sub {return ref($self) eq 'CODE'; }!
        => q!sub {return is_coderef($self); }!,

    # Eval tests
    q[eval q{ref $foo eq 'ARRAY'}] => q[eval q{is_arrayref($foo)}],
    q[eval q/ref $foo eq 'ARRAY'/] => q[eval q/is_arrayref($foo)/],
    q[eval "ref $foo eq 'ARRAY'"]  => q[eval "is_arrayref($foo)"]

    # not supported (yet?)
    #qq{ref(\$foo) # comment\nor}   => q{is_ref($foo) or # comment},
);

while ( my ( $input, $expect ) = splice @tests, 0, 2 ) {
    my $test_name = $input;
    my $output    = rewrite_string($input);

    is( $output, $expect, $test_name ); # or diag main::dump($output);

    last;
}

done_testing();
