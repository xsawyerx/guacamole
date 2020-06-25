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
    # Now you will get a warning if you don't break Standard Perl

=head1 WHERE'S THE REST

Soon...

=head1 SEE ALSO

L<Guacamole>
