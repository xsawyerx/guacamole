package standard;
# ABSTRACT: Enforce Standard Perl syntax with Guacamole

use strict;
use warnings;
use experimental qw< signatures >;
use Guacamole;
use Path::Tiny ();

sub import ( $class ) {
    my @caller      = caller();
    my $caller_file = $caller[1];
    my $content     = Path::Tiny::path($caller_file)->slurp_utf8();

    # Filter out everything past __DATA__ or __END__
    strip_terminators(\$content);

    # Strip POD, it isn't Perl
    strip_pods(\$content);

    eval { Guacamole->parse($content); }
    or do {
        print STDERR "File '$caller_file' does not pass Standard Perl.\n"
                   . "Parser says:\n"
                   . join '', map "> $_\n", split /\n/xms, $@;
    };

    return;
}

sub strip_terminators ($contentref) {
    ${$contentref} =~ s/^__DATA__\n.*//xms;
    ${$contentref} =~ s/^__END__\n.*//xms;
}

sub strip_pods ($contentref) {
    my @content_lines = split /\n/, ${$contentref};
    my $in_pod;
    foreach my $line (@content_lines) {
        if ( $line =~ /^=(?!cut)/ ) {
            $in_pod = 1;
        }

        $in_pod
            or next;

        $line =~ s{^}{#}xms;

        # This is after replace, so it's already commented out
        if ( $line =~ /^#=cut$/ ) {
            $in_pod = 0;
        }
    }

    ${$contentref} = join "\n", @content_lines;
}

1;

__END__

=pod

=head1 SYNOPSIS

    use standard;
    # Now you will get a warning if you don't conform to Standard Perl

=head1 DESCRIPTION

B<Standard Perl> aims to use only the syntax that makes Perl easy to parse.
A Perl static parser such as L<Guacamole> isn't that hard if we avoid a small
set of constructs that cause parser ambiguities.

These changes are described below. Over time, this documentation will explain
the standard itself versus the differences between this subset and what the
perl interpreter supports.

=head1 DIFFERENCES

=head2 Things not supported

This list covers constructs that the perl interpreter understands (for whatever
value of "understand" is) but that that Standard Perl does not support.

=over 4

=item * Auto-quoting

Perl's auto-quoting rules are... rather elaborate and awkward. Much of it is
unknown and can even depend on lowercase vs. uppercase and specific letters with
special meaning to the interpreter and not the user.

Thus, a string in Standard Perl is always quoted.

    $foo{key}   # not ok
    $foo{'key'} # ok

    %hash = ( foo   => 'bar' ); # not ok
    %hash = ( 'foo' => 'bar' ); # ok

=item * HEREDOCs

HEREDOCs are a monstrosity for parsers and cannot be expressed with a BNF. It is
thus not supported.

    # This will fail
    my $value = << '_END_OF_VALUE';
    ...
    _END_OF_VALUE

    # This is an alternative
    my $value =
    q{...
    };

    # This is another alternative:
    my $value = q{
    ...
    } =~ s/^\n//r;

=item * Indirect object notation

    my $instance = new Class;    # not ok
    my $instance = Class->new(); # ok

=item * Bareword filehandles

    open FOO, ...    # not ok
    open my $fh, ... # ok
    open $fh, ...    # ok

    print STDOUT $foo; # ok
    print STDERR $foo; # ok

    while ( <FOO>   ) {...} # not ok
    while ( <$foo>  ) {...} # ok
    while ( <STDIN> ) {...} # ok

The following bareword filehandles are supported:

=over 4

=item * C<STDIN>

=item * C<STDOUT>

=item * C<STDERR>

=item * C<ARGV>

=item * C<ARGVOUT>

=item * C<DATA>

=back

=item * Printing to filehandles with no brace

    print $fh $foo;   # not ok
    print {$fh} $foo; # ok


=item * C<_> outside file operations

    if ( -f $foo && -r _ ) {...} # ok
    print {$fh} _                # not ok

C<_> should only be used as a bareword identifier within C<-X> file
operations. In the C<print> example, Perl understands it as printing
an underscore character (C<"_">) to the filehandle, which is just
odd, so we do not support that.

=item * C<given> / C<when> / C<default>

Not supported.

=back

=head2 Things we changed

The following are limitations that Standard Perl has which the perl
interpreter doesn't.

=head3 Q-Like values

Q-Like values are one of the following: C<q>, C<qq>, C<qw>, C<qx>, C<qr>

However, the following limitations also apply to: C<m//>, C<s///> C<tr///>,
and C<y///>.

=over 4

=item * No nested delimiters

    $val = q< <> >;    # not ok
    $val = q< \<\> >;  # ok

If you want to use the delimiter within delimited space, escape it.

=item * Limited delimiters

Only the following delimiters are supported:

C<()>, C<[]>, C<{}>, C<< E<lt> E<gt> >>, C<//>, C<!!>, and C<||>.

    $val = q(...) # ok
    $val = q[...] # ok
    $val = q{...} # ok
    $val = q<...> # ok
    $val = q/.../ # ok
    $val = q!...! # ok
    $val = q|...| # ok

    $val = q@...@    # not ok
    $val = q#...#    # not ok
    $val = q Z ... Z # not ok

If you want to advocate for another set of delimiters, open a ticket.

=item * No spaces between before delimiters in Q-like values:

    q <foo>   # not ok
    q < foo > # not ok
    q ()      # not ok

    q<foo>    # ok
    q< foo >; # ok
    q()       # ok

=back

=head3 Subroutines

=over 4

=item * B<All> subroutines must use parentheses

    foo $bar   # not ok
    foo($bar)  # ok

There is an exception for methods:

    $foo->bar()         # ok
    $foo->bar           # ok

    $foo->bar()->baz()  # ok
    $foo->bar->baz      # ok

=item * Subroutines can have attributes and signatures

Standard Perl accepts both attributes and signatures.

=item * All subroutine prototypes must be declared using an attribute

    sub foo ($)           {...} # signature, not prototype
    sub foo :prototype($) {...} # prototype, not signature

=item * Prototypes do not change the parsing rules

    first {...} @foo         # not ok
    first( sub {...}, @foo ) # ok

We are looking into allowing developers to have their grammars hooking
up to the L<Guacamole> parser so it Standard Perl could be exnteded.
This will be useful for stuff like L<List::Util>, L<Dancer2>,
L<Mojolicious::Lite>, L<Moose>, etc.

Having said that, Standard Perl doesn't accept prototypes.

=back

=head3 Class names

=over 4

=item * Left of arrow is always an invocant, never a function

    Foo->new(); # always a class, never a function "Foo"

This is tricky because the perl interpreter might see a function called
C<Foo> in the same scope and call that instead. This would mean that
Standard Perl and the perl interpreter would report different results.

We have a shim layer in L<standard> that checks for this and alerts if
this will happen, so you never hit this issue when using C<standard>.

We advise other parsers who use Standard Perl BNF to include this part.

=item * Namespaces cannot end with a double colon

    Foo->bar();   # ok
    Foo::->bar(); # not ok

This might be changed in the future.

=back

=head3 Dereferencing

=over 4

=item * Prefixed dereferencing is only supported with braces

    @$foo    # not ok
    @{$foo}  # ok
    $foo->@* # ok

=back

=head3 Expressions

=over 4

=item * C<map> that attempts to return a pair must use parenthesis

    map {   $_ => 1   }, @foo  # not ok
    map { ( $_ => 1 ) }, @foo  # ok

=back

=head3 Eval

=over 4

=item * C<eval> only supports a block, not an expression

    eval { ... }   # ok
    eval " ... "   # not ok

=back

=head1 SEE ALSO

L<Guacamole>
